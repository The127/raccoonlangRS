use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
use crate::ast::statement::Statement;
use crate::parser::block_expression_node::{BlockExpressionNode, StatementKind};
use crate::source_map::{HasSpan, SourceCollection, Span};
use assert_matches::assert_matches;
use std::ops::DerefMut;
use ustr::Ustr;
use crate::ast::typing::TypeRef;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpression {
    pub(super) span_: Span,
    pub implicit: bool,
    pub let_: Option<LetDeclaration>,
    pub statements: Vec<Statement>,
    pub value: Option<Box<Expression>>,
}

impl HasSpan for BlockExpression {
    fn span(&self) -> Span {
        self.span_
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetDeclaration {
    span_: Span,
    pub binding: Ustr,
    pub value: Option<Box<Expression>>,
    pub type_ref: Option<TypeRef>,
}

impl LetDeclaration {
    pub fn new<S: Into<Span>>(span: S, binding: Ustr, value: Option<Expression>) -> Self {
        Self {
            span_: span.into(),
            binding,
            value: value.map(Box::new),
            type_ref: None,
        }
    }

    #[cfg(test)]
    pub fn with_type_ref(mut self, type_ref: TypeRef) -> Self {
        self.type_ref = Some(type_ref);
        self
    }
}

impl HasSpan for LetDeclaration {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_block_expression(
    node: &BlockExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    let value = node
        .value
        .as_ref()
        .map(|n| Box::new(transform_expression(n, sources)));

    let mut outer_block = BlockExpression {
        span_: node.span(),
        implicit: false,
        let_: None,
        statements: vec![],
        value: value,
    };

    let mut current_block = &mut outer_block;


    let mut is_first = true;

    for stmt in &node.statements {
        match &stmt.kind {
            StatementKind::Empty => (),
            StatementKind::Expression(expr) => {
                current_block.statements.push(Statement::Expression(transform_expression(expr, sources)));
                is_first = false;
            }
            StatementKind::Declaration(decl) => {
                let let_decl = LetDeclaration::new(
                    Span::empty(),
                    sources.get_identifier(decl.binding.unwrap().span()),
                    Some(transform_expression(decl.value.as_ref().unwrap(), sources)),
                );
                if is_first {
                    current_block.let_ = Some(let_decl)
                } else {
                    current_block.value = Some(Box::new(Expression {
                        kind: ExpressionKind::Block(BlockExpression {
                            span_: Default::default(),
                            implicit: true,
                            let_: Some(let_decl),
                            statements: vec![],
                            value: current_block.value.take(),
                        }),
                        type_ref: None,
                    }));

                    current_block = assert_matches!(
                        &mut (current_block.value.as_mut().unwrap().deref_mut().kind),
                        ExpressionKind::Block(x) => x);
                }
                is_first = false;
            }
        }

    }

    Expression {
        kind: ExpressionKind::Block(outer_block),
        type_ref: None,
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::block::{BlockExpression, LetDeclaration};
    use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
    use crate::ast::statement::Statement;
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::block_expression_node::{BlockExpressionNode, StatementNode};
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::let_declaration_node::LetDeclarationNode;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::{DecInteger, Identifier, Plus};
    use ustr::ustr;
    use crate::ast::path::Path;

    #[test]
    fn transform_empty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ }");

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(span, vec![], None));

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: span,
                    implicit: false,
                    let_: None,
                    statements: vec![],
                    value: None,
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_block_with_value() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ 123 }");
        let num_span = span.sub(2..5);

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            span,
            vec![],
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    num_span,
                    test_token!(DecInteger:num_span),
                    false,
                )),
            ))),
        ));

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: span,
                    implicit: false,
                    let_: None,
                    statements: vec![],
                    value: Some(Box::new(Expression::int_literal(num_span, 123))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_nested_block() {
        // arrange
        let mut sources = SourceCollection::new();
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
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: span,
                    implicit: false,
                    let_: None,
                    statements: vec![],
                    value: Some(Box::new(Expression::block(inner_span, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_block_with_starting_let() {
        // arrange
        let mut sources = SourceCollection::new();
        let span_a = sources.load_content("a");
        let span_1 = sources.load_content("1");

        // { let a = 1; }

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            Span::empty(),
            vec![StatementNode::decl(
                Span::empty(),
                LetDeclarationNode::new(
                    Span::empty(),
                    Some(test_token!(Identifier:span_a)),
                    Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                    ))),
                ),
            )],
            None,
        ));

        // act
        let block = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            block,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: Span::empty(),
                    implicit: false,
                    let_: Some(LetDeclaration::new(
                        Span::empty(),
                        ustr("a"),
                        Some(Expression::int_literal(span_1, 1),)
                    )),
                    statements: vec![],
                    value: None,
                }),
                type_ref: None
            }
        );
    }

    #[test]
    fn transform_block_with_starting_let_and_statement() {
        // arrange
        let mut sources = SourceCollection::new();
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
                        Some(test_token!(Identifier:span_a)),
                        Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                            IntegerLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
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
        let block = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            block,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: Span::empty(),
                    implicit: false,
                    let_: Some(LetDeclaration::new(
                        Span::empty(),
                        ustr("a"),
                        Some(Expression::int_literal(span_1, 1),)
                    )),
                    statements: vec![Statement::Expression(Expression::access(span_a, Path::name("a")))],
                    value: None,
                }),
                type_ref: None
            }
        );
    }

    #[test]
    fn transform_block_without_starting_let_with_statements() {
        // arrange
        let mut sources = SourceCollection::new();
        let span_1 = sources.load_content("1");
        let span_a = sources.load_content("a");

        // { 1; a; }

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            Span::empty(),
            vec![
                StatementNode::expr(
                    Span::empty(),
                    ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
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
        let block = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            block,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: Span::empty(),
                    implicit: false,
                    let_: None,
                    statements: vec![
                        Statement::Expression(Expression::int_literal(span_1, 1)),
                        Statement::Expression(Expression::access(span_a, Path::name("a"))),
                    ],
                    value: None,
                }),
                type_ref: None
            }
        );
    }

    #[test]
    fn transform_block_with_value_and_inner_let() {
        // arrange
        let mut sources = SourceCollection::new();
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
                    ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                    )),
                ),
                StatementNode::decl(
                    Span::empty(),
                    LetDeclarationNode::new(
                        Span::empty(),
                        Some(test_token!(Identifier:span_a)),
                        Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                            IntegerLiteralNode::new(
                                span_1,
                                test_token!(DecInteger:span_1),
                                false,
                            ),
                        ))),
                    ),
                ),
                StatementNode::decl(
                    Span::empty(),
                    LetDeclarationNode::new(
                        Span::empty(),
                        Some(test_token!(Identifier:span_b)),
                        Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                            IntegerLiteralNode::new(
                                span_2,
                                test_token!(DecInteger:span_2),
                                false,
                            ),
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
        let block = transform_expression(&block_node, &sources);


        // {1; { let a = 1; { let b = 2; a + b } } }


        // assert
        assert_eq!(
            block,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: Span::empty(),
                    implicit: false,
                    let_: None,
                    statements: vec![
                        Statement::Expression(Expression::int_literal(span_1, 1)),
                    ],
                    value: Some(Box::new(Expression::block_with_decl(
                        Span::empty(),
                        true,
                        LetDeclaration::new(Span::empty(), ustr("a"), Some(Expression::int_literal(span_1, 1))),
                        vec![],
                        Some(Expression::block_with_decl(
                            Span::empty(),
                            true,
                            LetDeclaration::new(Span::empty(), ustr("b"), Some(Expression::int_literal(span_2, 2))),
                            vec![],
                            Some(Expression::binary(
                                span_a + span_b,
                                BinaryOperator::Add,
                                Expression::access(span_a, Path::name("a")),
                                Expression::access(span_b, Path::name("b")),

                            ))
                        ))
                    ))),
                }),
                type_ref: None
            }
        );
    }
}
