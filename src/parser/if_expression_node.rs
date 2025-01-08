use crate::errors::Errors;
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::block_expression_node::{parse_block_expression, BlockExpressionNode};
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::treeizer::TokenTree;
use crate::until_iterator::until_iter;
use crate::{consume_token, group_starter};

#[derive(Debug, Eq, PartialEq)]
pub struct IfExpressionNode {
    span_: Span,
    condition: Option<Box<ExpressionNode>>,
    then: Option<Box<ExpressionNode>>,
    else_: Option<Box<ExpressionNode>>,
}

impl HasSpan for IfExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_if_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    let if_ = consume_token!(iter, If)?;

    group_starter!(block_starter, OpenCurly);
    let cond = {
        let mut sub_iter = marking(until_iter(iter, block_starter));
        parse_expression(&mut sub_iter, errors)
    };
    let then = parse_block_expression(iter, errors);

    let (else_token, else_block) = if let Some(else_token) = consume_token!(iter, Else) {
        let else_block = parse_expression(iter, errors);
        (Some(else_token), else_block)
    } else {
        (None, None)
    };

    Some(ExpressionNode::If(IfExpressionNode {
        span_: if_.span() + cond.span() + then.span() + else_token.span() + else_block.span(),
        condition: cond.map(Box::new),
        then: then.map(Box::new),
        else_: else_block.map(Box::new),
    }))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::ErrorKind;
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::tokenizer::TokenType::*;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_if_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_if_expression_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
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
    fn parse_if_expression_only_if() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(If:2..4, DecInteger:6..10, {:12, }:15);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::If(IfExpressionNode {
                span_: Span(2, 16),
                condition: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        6..10,
                        test_token!(DecInteger:6..10),
                        false
                    ))
                ))),
                then: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                    12..16,
                    None
                )))),
                else_: None,
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_if_expression_only_if_unexpected_group_before_then() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(If:2..4, DecInteger:6..10, (:12, ):13, {:15, }:16,);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::If(IfExpressionNode {
                span_: Span(2, 17),
                condition: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        6..10,
                        test_token!(DecInteger:6..10),
                        false
                    ))
                ))),
                then: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                    15..17,
                    None
                )))),
                else_: None,
            }))
        );
        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(12, ErrorKind::UnexpectedToken(OpenParen)));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_if_expression_if_else() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(If:2..4, DecInteger:6..10, {:12, }:15, Else:17, {:18, }:19 );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::If(IfExpressionNode {
                span_: Span(2, 20),
                condition: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        6..10,
                        test_token!(DecInteger:6..10),
                        false
                    ))
                ))),
                then: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                    12..16,
                    None
                )))),
                else_: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                    18..20,
                    None
                )))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_if_expression_if_else_if() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(If:2..4, DecInteger:6..10, {:12, }:15, Else:17..21, If:22..24, DecInteger:26..30, {:32, }:35 );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::If(IfExpressionNode {
                span_: Span(2, 35),
                condition: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        6..10,
                        test_token!(DecInteger:6..10),
                        false
                    ))
                ))),
                then: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                    12..16,
                    None
                )))),
                else_: Some(Box::new(ExpressionNode::If(IfExpressionNode{
                    span_: Span(22, 35),
                    condition: Some(Box::new(ExpressionNode::Literal(
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                            26..30,
                            test_token!(DecInteger:26..30),
                            false
                        ))
                    ))),
                    then: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                        32..35,
                        None
                    )))),
                    else_: None,
                }))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
