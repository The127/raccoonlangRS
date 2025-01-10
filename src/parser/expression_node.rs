use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::add_expression_node::{AddExpressionNode};
use crate::parser::block_expression_node::{parse_block_expression, BlockExpressionNode};
use crate::parser::compare_expression_node::{parse_compare_expression, CompareExpressionNode};
use crate::parser::if_expression_node::{parse_if_expression, IfExpressionNode};
use crate::parser::literal_expression_node::{parse_literal_expression, LiteralExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionNode {
    Literal(LiteralExpressionNode),
    Block(BlockExpressionNode),
    If(IfExpressionNode),
    Add(AddExpressionNode),
    Compare(CompareExpressionNode),
}

impl ExpressionNode {
    pub fn is_block(&self) -> bool {
        match self {
            ExpressionNode::Block(_) => true,
            ExpressionNode::If(_) => true,
            ExpressionNode::Literal(_) => false,
            ExpressionNode::Add(_) => false,
            ExpressionNode::Compare(_) => false,
        }
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
        }
    }
}

pub fn parse_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
    greedy_after_block: bool,
) -> Option<ExpressionNode> {
    Some(parse_compare_expression(iter, errors, greedy_after_block)?)
}

fn atom_expression_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    match iter.peek() {
        Some(TokenTree::Token(crate::tokenizer::Token {
            token_type: crate::tokenizer::TokenType::If,
            ..
        })) => true,
        _ => false,
    }
}

pub fn parse_atom_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
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
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::parser::literal_expression_node::IntegerLiteralNode;
    use crate::tokenizer::TokenType::{DecInteger, Unknown};
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn parse_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
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
