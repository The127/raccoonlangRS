use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::access_expression_node::{parse_access_expression, AccessExpressionNode};
use crate::parser::add_expression_node::AddExpressionNode;
use crate::parser::block_expression_node::{parse_block_expression, BlockExpressionNode};
use crate::parser::compare_expression_node::{parse_compare_expression, CompareExpressionNode};
use crate::parser::if_expression_node::{parse_if_expression, IfExpressionNode};
use crate::parser::literal_expression_node::{parse_literal_expression, LiteralExpressionNode};
use crate::parser::mul_expression_node::MulExpressionNode;
use crate::parser::new_expression_node::{parse_new_expression, NewExpressionNode};
use crate::parser::subsequent_expression_node::SubsequentExpressionNode;
use crate::parser::tuple_expression_node::{parse_tuple_expression, TupleExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::token_starter;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionNode {
    Literal(LiteralExpressionNode),
    Access(AccessExpressionNode),
    Block(BlockExpressionNode),
    If(IfExpressionNode),
    Add(AddExpressionNode),
    Mul(MulExpressionNode),
    Compare(CompareExpressionNode),
    Tuple(TupleExpressionNode),
    Subsequent(SubsequentExpressionNode),
    New(NewExpressionNode),
}

impl ExpressionNode {
    pub fn is_block(&self) -> bool {
        match self {
            ExpressionNode::Block(_) => true,
            ExpressionNode::If(_) => true,
            ExpressionNode::Literal(_) => false,
            ExpressionNode::Add(_) => false,
            ExpressionNode::Mul(_) => false,
            ExpressionNode::Compare(_) => false,
            ExpressionNode::Access(_) => false,
            ExpressionNode::Tuple(_) => false,
            ExpressionNode::Subsequent(_) => false,
            ExpressionNode::New(_) => false,
        }
    }
}

impl HasSpan for ExpressionNode {
    #[mutants::skip]
    fn span(&self) -> Span {
        match self {
            ExpressionNode::Literal(x) => x.span(),
            ExpressionNode::Block(x) => x.span(),
            ExpressionNode::If(x) => x.span(),
            ExpressionNode::Add(x) => x.span(),
            ExpressionNode::Mul(x) => x.span(),
            ExpressionNode::Compare(x) => x.span(),
            ExpressionNode::Access(x) => x.span(),
            ExpressionNode::Tuple(x) => x.span(),
            ExpressionNode::Subsequent(x) => x.span(),
            ExpressionNode::New(x) => x.span(),
        }
    }
}

fn expression_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    match iter.peek() {
        Some(TokenTree::Token(crate::tokenizer::Token {
            token_type: Identifier, // TODO: all expression starter tokens
            ..
        })) => true,
        _ => false,
    }
}

pub fn parse_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
    greedy_after_block: bool,
) -> Option<ExpressionNode> {
    Some(parse_compare_expression(iter, errors, greedy_after_block)?)
}

pub fn parse_atom_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    Some(
        parse_literal_expression(iter, errors)
            .or_else(|| parse_access_expression(iter, errors))
            .or_else(|| parse_tuple_expression(iter, errors))
            .or_else(|| parse_block_expression(iter, errors))
            .or_else(|| parse_if_expression(iter, errors))
            .or_else(|| parse_new_expression(iter, errors))?,
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::parser::add_expression_node::AddExpressionNodeFollow;
    use crate::parser::mul_expression_node::MulExpressionNodeFollow;
    use crate::parser::subsequent_expression_node::{
        SubsequentDotAccessNode, SubsequentExpressionFollowNode,
    };
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use assert_matches::assert_matches;

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
        errors.assert_empty();
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
        errors.assert_empty();
        assert_eq!(
            remaining,
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_expression_literal() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Literal(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_access() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Access(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_block() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({});
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Block(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_if() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(If, Identifier, {});
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::If(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_compare() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier, DoubleEquals, Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Compare(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_add() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier, Plus, Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Add(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_mul() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier, Asterisk, Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Mul(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_tuple() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((Identifier, Comma));
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Tuple(_)));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_after_compare() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Identifier, GreaterThan, Identifier, Plus, Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode::new(
                Span::empty(),
                Some(Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier),
                )))),
                test_token!(GreaterThan),
                Some(Box::new(ExpressionNode::Add(AddExpressionNode::new(
                    Span::empty(),
                    Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                        test_token!(Identifier),
                    ))),
                    vec![AddExpressionNodeFollow {
                        operator: test_token!(Plus),
                        operand: Some(ExpressionNode::Access(AccessExpressionNode::new(
                            test_token!(Identifier),
                        ))),
                    }],
                )))),
            ))),
        );
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn expression_precedence() {
        // TODO: always keep this function and the comment updated with all expressions!
        // compare -> add -> mul -> subsequent -> atom
        // above is the precedence order, from lowest to highest
        let input = test_tokentree!(
            Identifier,
            DoubleEquals,
            Identifier,
            Plus,
            Identifier,
            Asterisk,
            Identifier,
            Dot,
            Identifier,
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode::new(
                Span::empty(),
                Some(Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier),
                )))),
                test_token!(DoubleEquals),
                Some(Box::new(ExpressionNode::Add(AddExpressionNode::new(
                    Span::empty(),
                    Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                        test_token!(Identifier),
                    ))),
                    vec![AddExpressionNodeFollow {
                        operator: test_token!(Plus),
                        operand: Some(ExpressionNode::Mul(MulExpressionNode::new(
                            Span::empty(),
                            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                                test_token!(Identifier),
                            ))),
                            vec![MulExpressionNodeFollow {
                                operator: test_token!(Asterisk),
                                operand: Some(ExpressionNode::Subsequent(
                                    SubsequentExpressionNode::new(
                                        Span::empty(),
                                        Box::new(ExpressionNode::Access(
                                            AccessExpressionNode::new(test_token!(Identifier),)
                                        )),
                                        vec![SubsequentExpressionFollowNode::DotAccess(
                                            SubsequentDotAccessNode::new(
                                                Span::empty(),
                                                test_token!(Identifier)
                                            )
                                        )]
                                    )
                                )),
                            }]
                        ))),
                    },],
                )))),
            ))),
        );
        errors.assert_empty();
        assert!(remaining.is_empty());
    }
}
