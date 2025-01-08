use std::marker::PhantomData;
use crate::errors::Errors;
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::add_expression_node::parse_add_expression;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::recover_until;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType::*;
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::TokenTree;
use crate::{consume_token, expect_token, token_starter};
use crate::until_iterator::until_iter;

#[derive(Debug, Eq, PartialEq)]
pub struct CompareExpressionNode {
    span_: Span,
    pub left: Box<ExpressionNode>,
    pub operator: Token,
    pub right: Option<Box<ExpressionNode>>,
}

impl HasSpan for CompareExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_compare_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    let op_matcher = |t: &'a TokenTree| {
        matches!(
            t,
            TokenTree::Token(Token {
                token_type: DoubleEquals
                    | NotEquals
                    | LessThan
                    | LessOrEquals
                    | GreaterThan
                    | GreaterOrEquals,
                ..
            })
        )
    };

    let left = {
        let mut sub_iter = marking(Box::new(until_iter(iter, op_matcher)));
        let add = parse_add_expression(&mut sub_iter, errors);
        if add.is_some() {
            recover_until(&mut sub_iter, errors, [], []);
        }
        add
    };


    let mut follows = vec![];

    while let Some(op) = consume_token!(
        iter,
        DoubleEquals | NotEquals | LessThan | LessOrEquals | GreaterThan | GreaterOrEquals
    ) {
        let mut sub_iter = marking(until_iter(iter, op_matcher));
        let part = parse_add_expression(&mut sub_iter, errors);
        follows.push((op, part));
    }

    if follows.is_empty() {
        return left;
    }

    if follows.len() == 1 {
        let (op, right) = follows.pop().expect("checked non-empty");
        if let Some(left) = left {
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: left.span(),
                left: Box::new(left),
                operator: op,
                right: right.map(|x| Box::new(x)),
            }))
        } else {
            todo!("error");
        }
    } else {
        todo!("who knows");
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::marking_iterator::marking;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::{test_token, test_tokens, test_tokentree};

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
        assert_eq!(
            remaining,
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        );
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
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false),
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_equals() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(DecInteger:1..2, DoubleEquals:3..5, DecInteger:7..12);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: (1..12).into(),
                left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false),
                ))),
                operator: test_token!(DoubleEquals:3..5),
                right: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(7..12, test_token!(DecInteger:7..12), false))
                ))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
