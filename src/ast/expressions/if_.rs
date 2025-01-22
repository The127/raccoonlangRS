use crate::ast::expressions::{transform_expression, Expression};
use crate::errors::Errors;
use crate::parser::if_expression_node::IfExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression {
    pub(super) span_: Span,
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Option<Box<Expression>>,
}

impl HasSpan for IfExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_if_expression(
    node: &IfExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    Expression::if_(
        node.span(),
        node.condition
            .as_ref()
            .map(|x| transform_expression(x.as_ref(), errors, sources))
            .unwrap_or_else(|| Expression::unknown()),
        node.then
            .as_ref()
            .map(|x| transform_expression(x.as_ref(), errors, sources))
            .unwrap_or_else(|| Expression::unknown()),
        node.else_
            .as_ref()
            .map(|x| transform_expression(x.as_ref(), errors, sources)),
    )
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::if_::IfExpression;
    use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
    use crate::errors::Errors;
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::if_expression_node::IfExpressionNode;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::DecInteger;

    #[test]
    fn transform_if_with_else() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("if 1 {} else {}");

        let if_node = ExpressionNode::If(IfExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Number(NumberLiteralNode::new(
                    0,
                    test_token!(DecInteger:3),
                    false,
                )),
            ))),
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                5..7,
                vec![],
                None,
            )))),
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                14..16,
                vec![],
                None,
            )))),
        ));

        // act
        let expr = transform_expression(&if_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::If(IfExpression {
                    span_: span,
                    condition: Box::new(Expression::i32_literal(0, 1)),
                    then: Box::new(Expression::block(5..7, vec![], None)),
                    else_: Some(Box::new(Expression::block(14..16, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_if_without_condition() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("if {} else {}");

        let if_node = ExpressionNode::If(IfExpressionNode::new(
            span,
            None,
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                5..7,
                vec![],
                None,
            )))),
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                14..16,
                vec![],
                None,
            )))),
        ));

        // act
        let expr = transform_expression(&if_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::If(IfExpression {
                    span_: span,
                    condition: Box::new(Expression::unknown()),
                    then: Box::new(Expression::block(5..7, vec![], None)),
                    else_: Some(Box::new(Expression::block(14..16, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_if_without_then() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("if 1 else {}");

        let if_node = ExpressionNode::If(IfExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Number(NumberLiteralNode::new(
                    0,
                    test_token!(DecInteger:3),
                    false,
                )),
            ))),
            None,
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                14..16,
                vec![],
                None,
            )))),
        ));

        // act
        let expr = transform_expression(&if_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::If(IfExpression {
                    span_: span,
                    condition: Box::new(Expression::i32_literal(0, 1)),
                    then: Box::new(Expression::unknown()),
                    else_: Some(Box::new(Expression::block(14..16, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_if_without_else() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let mut errors = Errors::new();
        let span = sources.load_content("if 1 {}");

        let if_node = ExpressionNode::If(IfExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Number(NumberLiteralNode::new(
                    0,
                    test_token!(DecInteger:3),
                    false,
                )),
            ))),
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                5..7,
                vec![],
                None,
            )))),
            None,
        ));

        // act
        let expr = transform_expression(&if_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::If(IfExpression {
                    span_: span,
                    condition: Box::new(Expression::i32_literal(0, 1)),
                    then: Box::new(Expression::block(5..7, vec![], None)),
                    else_: None,
                }),
                type_ref: None,
            }
        );
    }
}
