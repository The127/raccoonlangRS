use crate::awesome_iterator::AwesomeIterator;
use crate::consume_token;
use crate::errors::Errors;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{BinInteger, HexInteger, OctInteger};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LiteralExpressionNode {
    Integer(IntegerLiteralNode),
    Boolean(BooleanLiteralNode),
}

impl LiteralExpressionNode {
    pub fn span(&self) -> Span {
        match self {
            LiteralExpressionNode::Integer(x) => x.span(),
            LiteralExpressionNode::Boolean(x) => x.span(),
        }
    }
}

pub fn parse_literal_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    parse_literal_bool_expression(iter, errors)
        .or_else(|| parse_literal_int_expression(iter, errors))
}

pub fn parse_literal_bool_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _: &mut Errors,
) -> Option<ExpressionNode> {
    consume_token!(iter, True | False).map(|token| {
        ExpressionNode::Literal(LiteralExpressionNode::Boolean(BooleanLiteralNode {
            span_: token.span(),
            value: token,
        }))
    })
}

pub fn parse_literal_int_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _: &mut Errors,
) -> Option<ExpressionNode> {
    let mut iter = iter.mark().auto_reset();

    let minus = consume_token!(&mut iter, Minus);
    let number = consume_token!(&mut iter, DecInteger | BinInteger | OctInteger | HexInteger)?;

    iter.discard();

    if minus.is_some() && matches!(number.token_type, BinInteger | OctInteger | HexInteger) {
        return None;
    }

    Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
        IntegerLiteralNode {
            negative: minus.is_some(),
            span_: minus.span() + number.span(),
            number: number,
        },
    )))
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct IntegerLiteralNode {
    span_: Span,
    pub number: Token,
    pub negative: bool,
}

impl IntegerLiteralNode {
    pub fn new<S: Into<Span>>(span: S, number: Token, negative: bool) -> Self {
        Self {
            span_: span.into(),
            number,
            negative,
        }
    }
}

impl HasSpan for IntegerLiteralNode {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct BooleanLiteralNode {
    span_: Span,
    pub value: Token,
}

impl BooleanLiteralNode {
    pub fn new<S: Into<Span>>(span: S, value: Token) -> Self {
        Self {
            span_: span.into(),
            value,
        }
    }
}

impl HasSpan for BooleanLiteralNode {
    fn span(&self) -> Span {
        self.span_
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::tokenizer::TokenType::{
        BinInteger, DecInteger, False, HexInteger, Minus, OctInteger, True, Unknown,
    };
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_literal_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
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
    fn parse_literal_positive_dec_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:5..15);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode {
                    span_: (5..15).into(),
                    negative: false,
                    number: test_token!(DecInteger:5..15),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_dec_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:3, DecInteger:5..15);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode {
                    span_: (3..15).into(),
                    negative: true,
                    number: test_token!(DecInteger:5..15),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_positive_bin_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(BinInteger:3..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode {
                    span_: (3..10).into(),
                    negative: false,
                    number: test_token!(BinInteger:3..10),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_bin_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:1, BinInteger:3..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_positive_oct_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(OctInteger:6..14);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode {
                    span_: (6..14).into(),
                    negative: false,
                    number: test_token!(OctInteger:6..14),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_oct_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:4, OctInteger:6..14);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_positive_hex_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(HexInteger:2..7);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode {
                    span_: (2..7).into(),
                    negative: false,
                    number: test_token!(HexInteger:2..7),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_hex_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:1, HexInteger:2..7);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_true() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(True:1..5);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Boolean(
                BooleanLiteralNode {
                    span_: Span(1, 5),
                    value: test_token!(True:1..5),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_false() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(False:1..6);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Boolean(
                BooleanLiteralNode {
                    span_: Span(1, 6),
                    value: test_token!(False:1..6),
                }
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
