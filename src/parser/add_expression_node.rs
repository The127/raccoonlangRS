use crate::errors::ErrorKind::MissingOperand;
use crate::errors::Errors;
use crate::awesome_iterator::AwesomeIterator;
use crate::parser::expression_node::{parse_atom_expression, ExpressionNode};
use crate::parser::literal_expression_node::{parse_literal_expression, LiteralExpressionNode};
use crate::parser::{recover_until};
use crate::source_map::{HasSpan, Span};
use crate::{token_starter, expect_token};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct AddExpressionNode {
    span_: Span,
    pub left: Box<ExpressionNode>,
    pub follows: Vec<AddExpressionNodeFollow>,
}

impl AddExpressionNode {
    pub fn new<S: Into<Span>>(span: S, left: Box<ExpressionNode>, follows: Vec<AddExpressionNodeFollow>) -> Self {
        Self {
            span_: span.into(),
            left: left,
            follows: follows,
        }
    }
}

impl HasSpan for AddExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AddExpressionNodeFollow {
    pub operator: Token,
    pub operand: Option<Box<ExpressionNode>>,
}

pub fn parse_add_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    if let Some(left) = parse_atom_expression(iter, errors) {
        token_starter!(op_plus, Plus);
        token_starter!(op_minus, Minus);
        if !recover_until(iter, errors, [op_plus, op_minus], []) {
            return Some(left);
        }

        let mut result = AddExpressionNode {
            span_: left.span(),
            left: Box::new(left),
            follows: vec![],
        };

        while recover_until(iter, errors, [op_plus, op_minus], []) {
            let operator_token = expect_token!(iter, Plus|Minus);
            result.span_ += operator_token.span();

            let right = if let Some(follow) = parse_atom_expression(iter, errors) {
                result.span_ += follow.span();
                Some(Box::new(follow))
            } else {
                errors.add(MissingOperand, result.span_.end());
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
    use crate::awesome_iterator::make_awesome;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_add_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
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
        let mut iter = make_awesome(input.iter());
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
            ))),
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_correct_syntax() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, Plus:4, BinInteger:8..20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span_: (1..20).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Plus:4),
                    operand: Some(Box::new(ExpressionNode::Literal(
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(8..20, test_token!(BinInteger:8..20), false))
                    ))),
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span_: (1..20).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Minus:4),
                    operand: Some(Box::new(ExpressionNode::Literal(
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(8..20, test_token!(BinInteger:8..20), false))
                    ))),
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span_: (1..25).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:4),
                        operand: Some(Box::new(ExpressionNode::Literal(
                            LiteralExpressionNode::Integer(IntegerLiteralNode::new(8..20, test_token!(BinInteger:8..20), false))
                        ))),
                    },
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:22),
                        operand: Some(Box::new(ExpressionNode::Literal(
                            LiteralExpressionNode::Integer(IntegerLiteralNode::new(24, test_token!(DecInteger:24), false))
                        ))),
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Add(AddExpressionNode {
                span_: (1..5).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Plus:4),
                    operand: None,
                }],
            }))
        );
        assert!(errors.has_error_at(5, MissingOperand));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
