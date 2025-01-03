use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::consume_group;
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::source_map::Span;
use crate::tokenizer::TokenType::OpenCurly;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct BlockExpression {
    pub span: Span,
    pub value: Option<Box<ExpressionNode>>,
}

pub fn parse_block_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<BlockExpression> {
    let group = consume_group(iter, OpenCurly)?;
    let mut iter = marking(group.children.iter());
    let value = parse_expression(&mut iter, errors);

    match iter.next() {
        Some(TokenTree::Token(token))=> errors.add(ErrorKind::UnexpectedToken(token.token_type), token.span),
        Some(TokenTree::Group(group)) => errors.add(ErrorKind::UnexpectedToken(group.open.token_type), group.open.span),
        _ => ()
    };

    Some(BlockExpression{
        span: group.span(),
        value: value.map(|e| Box::new(e)),
    })
}

#[cfg(test)]
mod test {
    use crate::errors::{ErrorKind, Errors};
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression::{IntegerLiteral, LiteralExpression};
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_expression_empty_input(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
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
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_empty_block(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, }:120);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(BlockExpression{
            span: (4..121).into(),
            value: None,
        }));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_just_value(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, }:120);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(BlockExpression{
            span: (4..121).into(),
            value: Some(Box::new(ExpressionNode::Literal(LiteralExpression::Integer(IntegerLiteral{
                span: (60..70).into(),
                number: test_token!(DecInteger:60..70),
                negative: false,
            })))),
        }));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_unexpected_token_after_value(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, Unknown:75..80 }:120);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(BlockExpression{
            span: (4..121).into(),
            value: Some(Box::new(ExpressionNode::Literal(LiteralExpression::Integer(IntegerLiteral{
                span: (60..70).into(),
                number: test_token!(DecInteger:60..70),
                negative: false,
            })))),
        }));
        assert!(errors.has_error_at(75..80, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}