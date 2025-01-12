use crate::ast::expressions::{transform_expression, Expression};
use crate::parser::tuple_expression_node::TupleExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TupleExpression {
    pub(super) span_: Span,
    pub values: Vec<Expression>,
}

impl HasSpan for TupleExpression {
    fn span(&self) -> Span {
        self.span_
    }
}
pub fn transform_tuple_expression(
    node: &TupleExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::tuple(node.span(), node.values.iter().map(|x| transform_expression(x, sources)).collect())
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::tuple::TupleExpression;
    use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::parser::tuple_expression_node::TupleExpressionNode;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::DecInteger;

    #[test]
    fn transform_tuple() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("(1,2,3)");
        let span_1 = span.sub(1..2);
        let span_2 = span.sub(3..4);
        let span_3 = span.sub(5..6);

        let tuple_node =
            ExpressionNode::Tuple(TupleExpressionNode::new(
                span,
                vec![
                    ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                    )),
                    ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_2, test_token!(DecInteger:span_2), false),
                    )),
                    ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_3, test_token!(DecInteger:span_3), false),
                    )),
                ],
            ));

        // act
        let expr = transform_expression(&tuple_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Tuple(TupleExpression {
                    span_: span,
                    values: vec![
                        Expression::int_literal(span_1, 1),
                        Expression::int_literal(span_2, 2),
                        Expression::int_literal(span_3, 3),
                    ],
                }),
                type_ref: None,
            }
        );
    }
}
