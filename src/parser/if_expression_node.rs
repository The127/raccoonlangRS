use crate::{consume_token, group_starter};
use crate::errors::Errors;
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::block_expression_node::{parse_block_expression, BlockExpressionNode};
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::TokenTree;
use crate::until_iterator::until_iter;

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
) -> Option<IfExpressionNode> {
    if let Some(if_) = consume_token!(iter, TokenType::If) {
        group_starter!(block_starter, OpenCurly);
        let cond = {
            let mut sub_iter = marking(until_iter(iter, block_starter));
            parse_expression(&mut sub_iter, errors)
        };
        let then = parse_block_expression(iter, errors);
        Some(IfExpressionNode {
            span_: if_.span() + cond.span() + then.span(),
            condition: cond.map(Box::new),
            then: then.map(Box::new),
            else_: None,
        })
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::*;
    use super::*;

    #[test]
    fn parse_if_expression_empty_input(){
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
    fn parse_if_expression_unknown_input(){
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
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_if_expression_only_if(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(If:2..4, DecInteger:6..10, {:12, }:15);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_if_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(IfExpressionNode{
            span_: Span(2, 16),
            condition: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode::new(6..10, test_token!(DecInteger:6..10), false))))),
            then: Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(12..16, None)))),
            else_: None,
        }));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}