use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct IfExpressionNode {

}

impl HasSpan for IfExpressionNode {
    fn span(&self) -> Span {
        todo!()
    }
}

pub fn parse_if_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<IfExpressionNode> {
    None
}

#[cfg(test)]
mod test {
    use crate::marking_iterator::marking;
    use crate::test_tokentree;
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

        }));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}