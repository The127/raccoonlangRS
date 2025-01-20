use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::mul_expression_node::parse_mul_expression;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;
use crate::{consume_token, seq_expression};

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
    pub operand: Option<ExpressionNode>,
}

seq_expression!(parse_add_expression, parse_mul_expression, Plus|Minus, Add, AddExpressionNode, AddExpressionNodeFollow);

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::literal_expression_node::{NumberLiteralNode, LiteralExpressionNode};
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use assert_matches::assert_matches;
    use crate::tokenizer::TokenType::*;

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
            Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
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
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Plus:4),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(8..20, test_token!(BinInteger:8..20), false)
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
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Minus:4),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                        NumberLiteralNode::new(8..20, test_token!(BinInteger:8..20), false)
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
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:4),
                        operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(8..20, test_token!(BinInteger:8..20), false)
                        ))),
                    },
                    AddExpressionNodeFollow {
                        operator: test_token!(Plus:22),
                        operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(24, test_token!(DecInteger:24), false)
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
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
                ))),
                follows: vec![AddExpressionNodeFollow {
                    operator: test_token!(Plus:4),
                    operand: None,
                }],
            }))
        );
        assert!(errors.has_error_at(5, ErrorKind::MissingOperand));
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
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
