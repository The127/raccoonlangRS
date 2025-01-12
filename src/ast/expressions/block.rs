use crate::ast::expressions::{transform_expression, Expression};
use crate::ast::statement::Statement;
use crate::parser::block_expression_node::BlockExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpression {
    pub(super) span_: Span,
    pub statements: Vec<Statement>,
    pub value: Option<Box<Expression>>,
}

impl HasSpan for BlockExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_block_expression(
    node: &BlockExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::block(
        node.span(),
        vec![],
        node.value
            .as_ref()
            .map(|n| transform_expression(n, sources)),
    )
}


#[cfg(test)]
mod test {
    use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
    use crate::ast::expressions::block::BlockExpression;
    use crate::parser::block_expression_node::{BlockExpressionNode, StatementNode};
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::let_declaration_node::LetDeclarationNode;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::DecInteger;

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
                    statements: vec![],
                    value: Some(Box::new(Expression::block(inner_span, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }


    // TODO: test transform block with statements!

}