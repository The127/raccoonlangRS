use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::add_expression_node::parse_add_expression;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::recover_until;
use crate::source_map::Span;
use crate::{consume_token, expect_token, token_starter};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct CompareExpressionNode {
    pub span: Span,
    pub left: Box<ExpressionNode>,
    pub operator: Token,
    pub right: Option<Box<ExpressionNode>>,
}

pub fn parse_compare_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    if let Some(left) = parse_add_expression(iter, errors) {
        token_starter!(op_equals, DoubleEquals);
        token_starter!(op_not_equals, NotEquals);
        token_starter!(op_less_than, LessThan);
        token_starter!(op_less_or_equals, LessOrEquals);
        token_starter!(op_greater_than, GreaterThan);
        token_starter!(op_greater_or_equals, GreaterOrEquals);
        if !recover_until(
            iter,
            errors,
            [
                op_equals,
                op_not_equals,
                op_less_or_equals,
                op_less_than,
                op_greater_or_equals,
                op_greater_than,
            ],
            [],
        ) {
            return Some(left);
        }

        let result = CompareExpressionNode {
            span: left.span(),
            left: Box::new(left),
            operator: expect_token!(iter, DoubleEquals|NotEquals|LessThan|LessOrEquals|GreaterThan|GreaterOrEquals),
            right: parse_add_expression(iter, errors).map(|x| Box::new(x)),
        };

        //TODO: error if recover to operator

        return Some(ExpressionNode::Compare(result));
    }

    None
}

#[cfg(test)]
mod test {
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::{test_token, test_tokens, test_tokentree};
    use super::*;

    #[test]
    fn parse_compare_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_just_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode{
                span: (1..2).into(),
                number: test_token!(DecInteger:1..2),
                negative: false,
            },
        ))));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_equals() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, DoubleEquals:3..5, DecInteger:7..12);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Compare(CompareExpressionNode{
            span: (1..12).into(),
            left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode{
                    span: (1..2).into(),
                    number: test_token!(DecInteger:1..2),
                    negative: false,
                },
            ))),
            operator: test_token!(DoubleEquals:3..5),
            right: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode{
                    span: (7..12).into(),
                    number: test_token!(DecInteger:7..12),
                    negative: false,
                },
            )))),
        })));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }


}
