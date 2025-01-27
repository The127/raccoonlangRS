use crate::ast::parse_transform::{transform_expression, Expression};
use crate::errors::Errors;
use crate::parser::tuple_expression_node::TupleExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

pub fn transform_tuple_expression(
    node: &TupleExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    Expression::tuple(
        node.span(),
        node.values
            .iter()
            .map(|x| transform_expression(x, errors, sources))
            .collect(),
    )
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::parse_transform::transform_expression;
    use crate::errors::Errors;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::parser::tuple_expression_node::TupleExpressionNode;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::DecInteger;

    #[test]
    fn transform_tuple() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("(1,2,3)");
        let span_1 = span.sub(1..2);
        let span_2 = span.sub(3..4);
        let span_3 = span.sub(5..6);

        let tuple_node = ExpressionNode::Tuple(TupleExpressionNode::new(
            span,
            vec![
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    span_1,
                    test_token!(DecInteger:span_1),
                    false,
                ))),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    span_2,
                    test_token!(DecInteger:span_2),
                    false,
                ))),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    span_3,
                    test_token!(DecInteger:span_3),
                    false,
                ))),
            ],
        ));

        // act
        let expr = transform_expression(&tuple_node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::tuple(
                span,
                vec![
                    Expression::i32_literal(span_1, 1),
                    Expression::i32_literal(span_2, 2),
                    Expression::i32_literal(span_3, 3),
                ]
            )
        );
    }
}
