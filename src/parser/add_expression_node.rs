use crate::errors::ErrorKind::MissingAddOperand;
use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::literal_expression_node::{parse_literal_expression, LiteralExpressionNode};
use crate::parser::{consume_token, recover_until};
use crate::source_map::Span;
use crate::token_starter;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct AddExpressionNode {
    pub span: Span,
    pub left: Box<ExpressionNode>,
    pub follows: Vec<AddExpressionNodeFollow>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AddExpressionNodeFollow {
    pub operator: Token,
    pub operand: Option<Box<ExpressionNode>>,
}

pub fn parse_add_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    if let Some(left) = parse_literal_expression(iter, errors) {
        token_starter!(op_plus, Plus);
        token_starter!(op_minus, Minus);
        if !recover_until(iter, errors, [op_plus, op_minus], []) {
            return Some(left);
        }

        let mut result = AddExpressionNode {
            span: left.span(),
            left: Box::new(left),
            follows: vec![],
        };

        while recover_until(iter, errors, [op_plus, op_minus], []) {
            let operator_token = consume_token(iter, Plus)
                .or_else(|| consume_token(iter, Minus))
                .expect("we just recovered to plus or minus");
            result.span += operator_token.span;

            let right = if let Some(follow) = parse_literal_expression(iter, errors){
                result.span += follow.span();
                Some(Box::new(follow))
            }else{
                errors.add(MissingAddOperand, result.span.end);
                None
            };

            result.follows.push(AddExpressionNodeFollow {
                operator: operator_token,
                operand: right,
            });
        }

        return Some(ExpressionNode::Add(result));
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_add_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_add_expression_just_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2,);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode {
                    span: (1..2).into(),
                    number: test_token!(DecInteger:1..2),
                    negative: false,
                }
            ))),
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_correct_syntax() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, Plus:4, BinInteger:8..20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span: (1..20).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode {
                        span: (1..2).into(),
                        number: test_token!(DecInteger:1..2),
                        negative: false,
                    }
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Plus:4),
                    operand: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode {
                            span: (8..20).into(),
                            number: test_token!(BinInteger:8..20),
                            negative: false,
                        }
                    )))),
                }],
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }


    #[test]
    fn parse_add_expression_correct_syntax_minus() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, Minus:4, BinInteger:8..20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span: (1..20).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode {
                        span: (1..2).into(),
                        number: test_token!(DecInteger:1..2),
                        negative: false,
                    }
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Minus:4),
                    operand: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode {
                            span: (8..20).into(),
                            number: test_token!(BinInteger:8..20),
                            negative: false,
                        }
                    )))),
                }],
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_multiple_plus() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(DecInteger:1..2, Plus:4, BinInteger:8..20, Plus:22, DecInteger:24);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span: (1..25).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode {
                        span: (1..2).into(),
                        number: test_token!(DecInteger:1..2),
                        negative: false,
                    }
                ))),
                follows: vec![
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:4),
                        operand: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                            IntegerLiteralNode {
                                span: (8..20).into(),
                                number: test_token!(BinInteger:8..20),
                                negative: false,
                            }
                        )))),
                    },
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:22),
                        operand: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                            IntegerLiteralNode {
                                span: 24.into(),
                                number: test_token!(DecInteger:24),
                                negative: false,
                            }
                        )))),
                    }
                ],
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_missing_right() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, Plus:4);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span: (1..5).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode {
                        span: (1..2).into(),
                        number: test_token!(DecInteger:1..2),
                        negative: false,
                    }
                ))),
                follows: vec![AddExpressionNodeFollow{
                    operator: test_token!(Plus:4),
                    operand: None,
                }],
            }))
        );
        assert!(errors.has_error_at(5, MissingAddOperand));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
