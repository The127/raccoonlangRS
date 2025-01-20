use crate::ast::expressions::{transform_expression, Expression};
use crate::parser::add_expression_node::AddExpressionNode;
use crate::parser::compare_expression_node::CompareExpressionNode;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::mul_expression_node::MulExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};
use crate::tokenizer::TokenType;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryExpression {
    pub(super) span_: Span,
    pub op: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Equals,
    NotEquals,
    LessThanOrEquals,
    GreaterThanOrEquals,
    LessThan,
    GreaterThan,
}

impl HasSpan for BinaryExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_compare_expression(
    node: &CompareExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::binary(
        node.span(),
        match node.operator.token_type {
            TokenType::DoubleEquals => BinaryOperator::Equals,
            TokenType::NotEquals => BinaryOperator::NotEquals,
            TokenType::LessThan => BinaryOperator::LessThan,
            TokenType::LessOrEquals => BinaryOperator::LessThanOrEquals,
            TokenType::GreaterThan => BinaryOperator::GreaterThan,
            TokenType::GreaterOrEquals => BinaryOperator::GreaterThanOrEquals,
            _ => unreachable!(),
        },
        match &node.left {
            Some(left) => transform_expression(left.as_ref(), sources),
            None => Expression::unknown(),
        },
        match &node.right {
            Some(right) => transform_expression(right.as_ref(), sources),
            None => Expression::unknown(),
        },
    )
}


fn map_op(token_type: TokenType) -> BinaryOperator {
    match token_type {
        TokenType::Plus => BinaryOperator::Add,
        TokenType::Minus => BinaryOperator::Sub,
        TokenType::Asterisk => BinaryOperator::Mul,
        TokenType::Slash => BinaryOperator::Div,
        _ => unreachable!(),
    }
}

fn map_expr(node: Option<&ExpressionNode>, sources: &SourceCollection) -> Expression {
    match node {
        Some(expr) => transform_expression(expr, sources),
        None => Expression::unknown(),
    }
}

pub fn transform_add_expression(
    node: &AddExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    let mut result = map_expr(Some(&node.left), sources);

    for follow in &node.follows {
        result = Expression::binary(
            result.span() + follow.operator.span() + follow.operand.span(),
            map_op(follow.operator.token_type),
            result,
            map_expr(follow.operand.as_ref(), sources),
        );
    }

    result
}

pub fn transform_mul_expression(
    node: &MulExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    let mut result = map_expr(Some(&node.left), sources);

    for follow in &node.follows {
        result = Expression::binary(
            result.span() + follow.operator.span() + follow.operand.span(),
            map_op(follow.operator.token_type),
            result,
            map_expr(follow.operand.as_ref(), sources),
        );
    }

    result
}

#[cfg(test)]
mod test {
    use parameterized::parameterized;
    use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
    use crate::ast::expressions::binary::{BinaryExpression, BinaryOperator};
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::compare_expression_node::CompareExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{NumberLiteralNode, LiteralExpressionNode};
    use crate::parser::mul_expression_node::{MulExpressionNode, MulExpressionNodeFollow};
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::{Token, TokenType};
    use crate::tokenizer::TokenType::{Asterisk, DecInteger, Minus, Plus, Slash};

    #[test]
    fn transform_single_add() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 + 2");

        let add_node = ExpressionNode::Add(AddExpressionNode::new(
            span,
            Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                NumberLiteralNode::new(0, test_token!(DecInteger:0), false),
            ))),
            vec![AddExpressionNodeFollow {
                operator: test_token!(Plus:2),
                operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(4, test_token!(DecInteger:4), false),
                ))),
            }],
        ));

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span,
                    op: BinaryOperator::Add,
                    left: Box::new(Expression::i32_literal(0, 1)),
                    right: Box::new(Expression::i32_literal(4, 2)),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_multi_add() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 + 2 - 3");

        let span_1 = span.sub(0..1);
        let span_plus = span.sub(2..3);
        let span_2 = span.sub(4..5);
        let span_minus = span.sub(6..7);
        let span_3 = span.sub(8..9);

        let add_node = ExpressionNode::Add(AddExpressionNode::new(
            span,
            Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
            ))),
            vec![
                AddExpressionNodeFollow {
                    operator: test_token!(Plus:span_plus),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(span_2, test_token!(DecInteger:span_2), false),
                    ))),
                },
                AddExpressionNodeFollow {
                    operator: test_token!(Minus:span_minus),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(span_3, test_token!(DecInteger:span_3), false),
                    ))),
                },
            ],
        ));

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span_1 + span_3,
                    op: BinaryOperator::Sub,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Binary(BinaryExpression {
                            span_: span_1 + span_2,
                            op: BinaryOperator::Add,
                            left: Box::new(Expression::i32_literal(span_1, 1)),
                            right: Box::new(Expression::i32_literal(span_2, 2)),
                        }),
                        type_ref: None,
                    }),
                    right: Box::new(Expression::i32_literal(span_3, 3)),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_single_mul() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 * 2");

        let mul_node = ExpressionNode::Mul(MulExpressionNode::new(
            span,
            Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                NumberLiteralNode::new(0, test_token!(DecInteger:0), false),
            ))),
            vec![MulExpressionNodeFollow {
                operator: test_token!(Asterisk:2),
                operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(4, test_token!(DecInteger:4), false),
                ))),
            }],
        ));

        // act
        let expr = transform_expression(&mul_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span,
                    op: BinaryOperator::Mul,
                    left: Box::new(Expression::i32_literal(0, 1)),
                    right: Box::new(Expression::i32_literal(4, 2)),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_multi_mul() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 * 2 / 3");

        let span_1 = span.sub(0..1);
        let span_mul = span.sub(2..3);
        let span_2 = span.sub(4..5);
        let span_div = span.sub(6..7);
        let span_3 = span.sub(8..9);

        let add_node = ExpressionNode::Mul(MulExpressionNode::new(
            span,
            Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
            ))),
            vec![
                MulExpressionNodeFollow {
                    operator: test_token!(Asterisk:span_mul),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(span_2, test_token!(DecInteger:span_2), false),
                    ))),
                },
                MulExpressionNodeFollow {
                    operator: test_token!(Slash:span_div),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(span_3, test_token!(DecInteger:span_3), false),
                    ))),
                },
            ],
        ));

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span_1 + span_3,
                    op: BinaryOperator::Div,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Binary(BinaryExpression {
                            span_: span_1 + span_2,
                            op: BinaryOperator::Mul,
                            left: Box::new(Expression::i32_literal(span_1, 1)),
                            right: Box::new(Expression::i32_literal(span_2, 2)),
                        }),
                        type_ref: None,
                    }),
                    right: Box::new(Expression::i32_literal(span_3, 3)),
                }),
                type_ref: None,
            }
        );
    }

    #[parameterized(values = {
        (TokenType::DoubleEquals, BinaryOperator::Equals),
        (TokenType::NotEquals, BinaryOperator::NotEquals),
        (TokenType::LessOrEquals, BinaryOperator::LessThanOrEquals),
        (TokenType::GreaterOrEquals, BinaryOperator::GreaterThanOrEquals),
        (TokenType::LessThan, BinaryOperator::LessThan),
        (TokenType::GreaterThan, BinaryOperator::GreaterThan),
    })]
    fn transform_compare(values: (TokenType, BinaryOperator)) {
        let (op_token, op_expected) = values;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 ·· 2");
        let span_1 = span.sub(0..1);
        let span_op = span.sub(2..4);
        let span_2 = span.sub(5..6);

        let compare_node = ExpressionNode::Compare(CompareExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Number(NumberLiteralNode::new(
                    span_1,
                    test_token!(DecInteger:span_1),
                    false,
                )),
            ))),
            Token::new(span_op, op_token),
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Number(NumberLiteralNode::new(
                    span_2,
                    test_token!(DecInteger:span_2),
                    false,
                )),
            ))),
        ));

        // act
        let expr = transform_expression(&compare_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span,
                    op: op_expected,
                    left: Box::new(Expression::i32_literal(span_1, 1)),
                    right: Box::new(Expression::i32_literal(span_2, 2)),
                }),
                type_ref: None,
            }
        );
    }

}