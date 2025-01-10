use crate::awesome_iterator::AwesomeIterator;
use crate::errors::ErrorKind::MissingOperand;
use crate::errors::Errors;
use crate::parser::expression_node::{parse_atom_expression, ExpressionNode};
use crate::parser::recover_until;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;
use crate::{expect_token, token_starter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AddExpressionNode {
    span_: Span,
    pub left: Box<ExpressionNode>,
    pub follows: Vec<AddExpressionNodeFollow>,
}

impl AddExpressionNode {
    pub fn new<S: Into<Span>>(
        span: S,
        left: Box<ExpressionNode>,
        follows: Vec<AddExpressionNodeFollow>,
    ) -> Self {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AddExpressionNodeFollow {
    pub operator: Token,
    pub operand: Option<Box<ExpressionNode>>,
}

pub fn parse_add_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
    greedy_after_block: bool,
) -> Option<ExpressionNode> {
    // TODO: left can be None, and still continue parsing
    if let Some(left) = parse_atom_expression(iter, errors) {
        if left.is_block() && !greedy_after_block {
            return Some(left);
        }

        token_starter!(operator, Plus|Minus);

        let mut recover_errors = Errors::new();
        let mut mark = iter.mark();

        if !recover_until(&mut mark, &mut recover_errors, [operator], []) {
            mark.reset();
            return Some(left);
        }
        mark.discard();
        errors.merge(recover_errors);

        let mut result = AddExpressionNode {
            span_: left.span(),
            left: Box::new(left),
            follows: vec![],
        };

        loop {
            let mut mark = iter.mark();
            let mut recover_errors = Errors::new();
            if !recover_until(&mut mark, &mut recover_errors, [operator], []) {
                mark.reset();
                break;
            }
            mark.discard();
            errors.merge(recover_errors);

            let operator_token = expect_token!(iter, Plus | Minus);
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
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use assert_matches::assert_matches;

    #[test]
    fn parse_add_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors, false);
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
        let result = parse_add_expression(&mut iter, &mut errors, false);
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
        let result = parse_add_expression(&mut iter, &mut errors, false);
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
        let result = parse_add_expression(&mut iter, &mut errors, false);
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
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                            8..20,
                            test_token!(BinInteger:8..20),
                            false
                        ))
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
        let result = parse_add_expression(&mut iter, &mut errors, false);
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
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                            8..20,
                            test_token!(BinInteger:8..20),
                            false
                        ))
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
        let result = parse_add_expression(&mut iter, &mut errors, false);
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
                            LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                                8..20,
                                test_token!(BinInteger:8..20),
                                false
                            ))
                        ))),
                    },
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:22),
                        operand: Some(Box::new(ExpressionNode::Literal(
                            LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                                24,
                                test_token!(DecInteger:24),
                                false
                            ))
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
        let result = parse_add_expression(&mut iter, &mut errors, false);
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

    #[test]
    fn parse_add_nongreedy_after_block() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({}, Plus, DecInteger);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Block(_)));

        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Plus, DecInteger).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_add_always_greedy_after_second_operand() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger, Plus, {}, Plus, DecInteger);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Add(_)));

        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_greedy_after_block() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({}, Plus, DecInteger);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_add_expression(&mut iter, &mut errors, true);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Add(_)));

        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!().iter().collect::<Vec<_>>()
        );
    }
}
