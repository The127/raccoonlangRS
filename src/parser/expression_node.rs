use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::block_expression::BlockExpression;
use crate::parser::literal_expression::{parse_literal_expression, LiteralExpression};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionNode{
    Literal(LiteralExpression),
    Block(BlockExpression),
}

pub fn parse_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    Some(ExpressionNode::Literal(parse_literal_expression(iter, errors)?))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression::IntegerLiteral;
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::{DecInteger, Unknown};
    use crate::treeizer::TokenTree;

    #[test]
    fn parse_expression_empty_input(){
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
    fn parse_expression_unknown_input(){
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
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_literal(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:2..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Literal(LiteralExpression::Integer(IntegerLiteral{
            span: (2..10).into(),
            negative: false,
            number: test_token!(DecInteger:2..10),
        }))));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

}