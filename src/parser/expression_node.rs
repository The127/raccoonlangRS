use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::add_expression_node::{parse_add_expression, AddExpressionNode};
use crate::parser::block_expression_node::{parse_block_expression, BlockExpressionNode};
use crate::parser::compare_expression_node::CompareExpressionNode;
use crate::parser::expression_node::ExpressionNode::Unknown;
use crate::parser::if_expression_node::{parse_if_expression, IfExpressionNode};
use crate::parser::literal_expression_node::{parse_literal_expression, LiteralExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionNode {
    Literal(LiteralExpressionNode),
    Block(BlockExpressionNode),
    If(IfExpressionNode),
    Add(AddExpressionNode),
    Compare(CompareExpressionNode),
    Unknown,
}

impl ExpressionNode {
    pub fn unknown() -> Self {
        Unknown
    }
}

impl HasSpan for ExpressionNode {
    fn span(&self) -> Span {
        match self {
            ExpressionNode::Literal(x) => x.span(),
            ExpressionNode::Block(x) => x.span(),
            ExpressionNode::If(x) => x.span(),
            ExpressionNode::Add(x) => x.span(),
            ExpressionNode::Compare(x) => x.span(),
            Unknown => Span::empty(),
        }
    }
}

pub fn parse_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    Some(parse_add_expression(iter, errors)?)
}

pub fn parse_atom_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    Some(
        parse_literal_expression(iter, errors)
            .or_else(|| parse_block_expression(iter, errors))
            .or_else(|| parse_if_expression(iter, errors))?,
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression_node::IntegerLiteralNode;
    use crate::tokenizer::TokenType::{DecInteger, Unknown};
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors);
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
    fn parse_expression_literal() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:2..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(2..10, test_token!(DecInteger:2..10), false)
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
