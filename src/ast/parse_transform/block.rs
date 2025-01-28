use crate::add_error;
use crate::ast::expressions::block::LetDeclaration;
use crate::ast::parse_transform::{transform_expression, Expression};
use crate::ast::statement::Statement;
use crate::errors::Errors;
use crate::parser::block_expression_node::{BlockExpressionNode, StatementKind};
use crate::source_map::{HasSpan, SourceCollection, Span};
use assert_matches::assert_matches;
use std::ops::DerefMut;
use crate::ast::expressions::ExpressionKind;
use crate::ast::parse_transform::pattern::transform_pattern;

pub fn transform_block_expression(
    node: &BlockExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    let value = node
        .value
        .as_ref()
        .map(|n| transform_expression(n, errors, sources));

    let mut outer_block = Expression::block(node.span(), vec![], value);

    let mut current_block = assert_matches!(&mut outer_block.kind, ExpressionKind::Block(x) => x);

    let mut is_first = true;

    for stmt in &node.statements {
        match &stmt.kind {
            StatementKind::Empty => (),
            StatementKind::Expression(expr) => {
                current_block
                    .statements
                    .push(Statement::Expression(transform_expression(
                        expr, errors, sources,
                    )));
                is_first = false;
            }
            StatementKind::Declaration(decl) => {
                let value = if let Some(v) = decl.value.as_ref() {
                    transform_expression(v, errors, sources)
                } else {
                    add_error!(errors, decl.span(), MissingLetDeclarationValue);
                    Expression::unknown()
                };

                let let_decl = LetDeclaration::new(
                    decl.span(),
                    transform_pattern(decl.binding.as_ref().unwrap(), sources),
                    value,
                );
                if is_first {
                    current_block.let_ = Some(let_decl)
                } else {
                    current_block.value = Some(Box::new(
                        Expression::block_with_decl(
                            Span::empty(), // TODO: this span looks wrong, make sure to test it!
                            true,
                            let_decl,
                            vec![],
                            Some(*(current_block.value.take().unwrap())),
                        )
                    ));

                    current_block = assert_matches!(
                        &mut (current_block.value.as_mut().unwrap().deref_mut().kind),
                        ExpressionKind::Block(x) => x);
                }
                is_first = false;
            }
        }
    }

    outer_block
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::Expression;
    use crate::ast::parse_transform::transform_expression;
    use crate::ast::path::Path;
    use crate::ast::pattern::Pattern;
    use crate::ast::statement::Statement;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::block_expression_node::{BlockExpressionNode, StatementNode};
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::let_declaration_node::LetDeclarationNode;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::parser::pattern_node::PatternNode;
    use crate::parser::ToSpanned;
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::{DecInteger, Identifier, Plus};
    use ustr::ustr;

    #[test]
    fn transform_empty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("{ }");

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(span, vec![], None));

        // act
        let expr = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::block(span, vec![], None),);
    }

    #[test]
    fn transform_block_with_value() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("{ 123 }");
        let num_span = span.sub(2..5);

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            span,
            vec![],
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Number(NumberLiteralNode::new(
                    num_span,
                    test_token!(DecInteger:num_span),
                    false,
                )),
            ))),
        ));

        // act
        let expr = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::block(span, vec![], Some(Expression::i32_literal(num_span, 123)),)
        );
    }

    #[test]
    fn transform_nested_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("{ { } }");
        let inner_span: Span = span.sub(2..5);

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            span,
            vec![],
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                inner_span,
                vec![],
                None,
            )))),
        ));

        // act
        let expr = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::block(
                span,
                vec![],
                Some(Expression::block(inner_span, vec![], None)),
            )
        );
    }

    #[test]
    fn transform_block_with_starting_let() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span_a = sources.load_content("a");
        let span_1 = sources.load_content("1");

        // { let a = 1; }

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            Span::empty(),
            vec![StatementNode::decl(
                Span::empty(),
                LetDeclarationNode::new(
                    Span::empty(),
                    Some(PatternNode::Name(test_token!(Identifier:span_a))),
                    Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                    ))),
                ),
            )],
            None,
        ));

        // act
        let block = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            block,
            Expression::block_with_decl(
                Span::empty(),
                false,
                LetDeclaration::new(
                    Span::empty(),
                    Pattern::Name(ustr("a")),
                    Expression::i32_literal(span_1, 1),
                ),
                vec![],
                None,
            )
        );
    }

    #[test]
    fn transform_block_with_starting_let_and_statement() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span_a = sources.load_content("a");
        let span_1 = sources.load_content("1");

        // { let a = 1; a; }

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            Span::empty(),
            vec![
                StatementNode::decl(
                    Span::empty(),
                    LetDeclarationNode::new(
                        Span::empty(),
                        Some(PatternNode::Name(test_token!(Identifier:span_a))),
                        Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                        ))),
                    ),
                ),
                StatementNode::expr(
                    Span::empty(),
                    ExpressionNode::Access(AccessExpressionNode::new(
                        test_token!(Identifier:span_a),
                    )),
                ),
            ],
            None,
        ));

        // act
        let block = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            block,
            Expression::block_with_decl(
                Span::empty(),
                false,
                LetDeclaration::new(
                    Span::empty(),
                    Pattern::Name(ustr("a")),
                    Expression::i32_literal(span_1, 1),
                ),
                vec![Statement::Expression(Expression::access(
                    span_a,
                    Path::name("a")
                ))],
                None,
            )
        );
    }

    #[test]
    fn transform_block_without_starting_let_with_statements() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span_1 = sources.load_content("1");
        let span_a = sources.load_content("a");

        // { 1; a; }

        let block_node =
            ExpressionNode::Block(BlockExpressionNode::new(
                Span::empty(),
                vec![
                    StatementNode::expr(
                        Span::empty(),
                        ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                        )),
                    ),
                    StatementNode::expr(
                        Span::empty(),
                        ExpressionNode::Access(AccessExpressionNode::new(
                            test_token!(Identifier:span_a),
                        )),
                    ),
                ],
                None,
            ));

        // act
        let block = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            block,
            Expression::block(
                Span::empty(),
                vec![
                    Statement::Expression(Expression::i32_literal(span_1, 1)),
                    Statement::Expression(Expression::access(span_a, Path::name("a"))),
                ],
                None,
            )
        );
    }

    #[test]
    fn transform_block_with_value_and_inner_let() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span_1 = sources.load_content("1");
        let span_2 = sources.load_content("2");
        let span_a = sources.load_content("a");
        let span_b = sources.load_content("b");

        // { 1; let a = 1; let b = 2; a + b }

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            Span::empty(),
            vec![
                StatementNode::expr(
                    Span::empty(),
                    ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                        span_1,
                        test_token!(DecInteger:span_1),
                        false,
                    ))),
                ),
                StatementNode::decl(
                    Span::empty(),
                    LetDeclarationNode::new(
                        Span::empty(),
                        Some(PatternNode::Name(test_token!(Identifier:span_a))),
                        Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                        ))),
                    ),
                ),
                StatementNode::decl(
                    Span::empty(),
                    LetDeclarationNode::new(
                        Span::empty(),
                        Some(PatternNode::Name(test_token!(Identifier:span_b))),
                        Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(span_2, test_token!(DecInteger:span_2), false),
                        ))),
                    ),
                ),
            ],
            Some(Box::new(ExpressionNode::Add(AddExpressionNode::new(
                Span::empty(),
                Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier:span_a),
                ))),
                vec![AddExpressionNodeFollow {
                    operator: test_token!(Plus),
                    operand: Some(ExpressionNode::Access(AccessExpressionNode::new(
                        test_token!(Identifier:span_b),
                    ))),
                }],
            )))),
        ));

        // act
        let block = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            block,
            Expression::block(
                Span::empty(),
                vec![Statement::Expression(Expression::i32_literal(span_1, 1)),],
                Some(Expression::block_with_decl(
                    Span::empty(),
                    true,
                    LetDeclaration::new(
                        Span::empty(),
                        Pattern::Name(ustr("a")),
                        Expression::i32_literal(span_1, 1)
                    ),
                    vec![],
                    Some(Expression::block_with_decl(
                        Span::empty(),
                        true,
                        LetDeclaration::new(
                            Span::empty(),
                            Pattern::Name(ustr("b")),
                            Expression::i32_literal(span_2, 2)
                        ),
                        vec![],
                        Some(Expression::binary(
                            span_a + span_b,
                            BinaryOperator::Add.spanned_empty(),
                            Expression::access(span_a, Path::name("a")),
                            Expression::access(span_b, Path::name("b")),
                        ))
                    ))
                ))
            )
        );
    }

    #[test]
    fn transform_let_without_value() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span_let = sources.load_content("let a");
        let span_a = span_let.sub(4..);

        // { let a; }

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            span_let,
            vec![StatementNode::decl(
                span_let,
                LetDeclarationNode::new(
                    span_let,
                    Some(PatternNode::Name(test_token!(Identifier:span_a))),
                    None,
                ),
            )],
            None,
        ));

        // act
        let block = transform_expression(&block_node, &mut errors, &sources);

        // assert
        assert_eq!(
            block,
            Expression::block_with_decl(
                span_let,
                false,
                LetDeclaration::new(span_let, Pattern::Name(ustr("a")), Expression::unknown(),),
                vec![],
                None,
            )
        );
        assert!(errors.has_error_at(span_let, ErrorKind::MissingLetDeclarationValue));
        assert_eq!(errors.get_errors().len(), 1);
    }
}
