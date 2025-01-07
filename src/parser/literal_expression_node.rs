use crate::consume_token;
use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::Span;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{BinInteger, DecInteger, HexInteger, Minus, OctInteger};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LiteralExpressionNode {
    Integer(IntegerLiteralNode),
}

impl LiteralExpressionNode {
    pub fn span(&self) -> Span {
        match self {
            LiteralExpressionNode::Integer(x) => x.span,
        }
    }
}

pub fn parse_literal_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    _: &mut Errors,
) -> Option<ExpressionNode> {
    let mut iter = iter.mark().auto_reset();

    let minus = consume_token!(&mut iter, Minus);
    let number = consume_token!(&mut iter, DecInteger|BinInteger|OctInteger|HexInteger)?;

    iter.discard();

    if minus.is_some() && matches!(number.token_type, BinInteger | OctInteger | HexInteger) {
        return None;
    }

    Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
        negative: minus.is_some(),
        span: minus
            .and_then(|t| Some(t.span + number.span))
            .unwrap_or(number.span),
        number: number,
    })))
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct IntegerLiteralNode {
    pub span: Span,
    pub number: Token,
    pub negative: bool,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::tokenizer::TokenType::{
        BinInteger, DecInteger, HexInteger, Minus, OctInteger, Unknown,
    };
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_literal_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span: (5..15).into(),
                negative: false,
                number: test_token!(DecInteger:5..15),
            })))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_dec_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:3, DecInteger:5..15);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span: (3..15).into(),
                negative: true,
                number: test_token!(DecInteger:5..15),
            })))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_positive_bin_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(BinInteger:3..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span: (3..10).into(),
                negative: false,
                number: test_token!(BinInteger:3..10),
            })))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_bin_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:1, BinInteger:3..10);
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span: (6..14).into(),
                negative: false,
                number: test_token!(OctInteger:6..14),
            })))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_oct_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:4, OctInteger:6..14);
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span: (2..7).into(),
                negative: false,
                number: test_token!(HexInteger:2..7),
            })))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_literal_negative_hex_integer() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Minus:1, HexInteger:2..7);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_literal_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
