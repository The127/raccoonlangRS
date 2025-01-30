use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::Errors;
use crate::parser::expression_node::{parse_atom_expression, parse_expression, ExpressionNode};
use crate::parser::{consume_group, recover_until};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{OpenParen, OpenSquare};
use crate::treeizer::{Group, TokenTree};
use crate::{add_error, consume_token, token_starter};
use crate::parser::arg_node::{parse_args, ArgNode, NamedArgNode, UnnamedArgNode};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentExpressionNode {
    span_: Span,
    pub left: Box<ExpressionNode>,
    pub follows: Vec<SubsequentExpressionFollowNode>,
}

impl HasSpan for SubsequentExpressionNode {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}

impl SubsequentExpressionNode {
    pub fn new<S: Into<Span>>(
        span: S,
        left: Box<ExpressionNode>,
        follows: Vec<SubsequentExpressionFollowNode>,
    ) -> Self {
        Self {
            span_: span.into(),
            left: left,
            follows,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SubsequentExpressionFollowNode {
    CallLike(SubsequentCallLikeNode),
    DotAccess(SubsequentDotAccessNode),
}

impl HasSpan for SubsequentExpressionFollowNode {
    #[mutants::skip]
    fn span(&self) -> Span {
        match self {
            SubsequentExpressionFollowNode::CallLike(x) => x.span(),
            SubsequentExpressionFollowNode::DotAccess(x) => x.span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum CallLikeType {
    Call,
    Index,
    With,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentCallLikeNode {
    span_: Span,
    pub type_: CallLikeType,
    pub args: Vec<ArgNode>,
}

impl HasSpan for SubsequentCallLikeNode {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}

impl SubsequentCallLikeNode {
    pub fn call<S: Into<Span>>(span: S, args: Vec<ArgNode>) -> Self {
        Self {
            span_: span.into(),
            type_: CallLikeType::Call,
            args,
        }
    }

    pub fn index<S: Into<Span>>(span: S, args: Vec<ArgNode>) -> Self {
        Self {
            span_: span.into(),
            type_: CallLikeType::Index,
            args,
        }
    }

    pub fn with<S: Into<Span>>(span: S, args: Vec<ArgNode>) -> Self {
        Self {
            span_: span.into(),
            type_: CallLikeType::With,
            args,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentDotAccessNode {
    span_: Span,
    pub name: Token,
}

impl SubsequentDotAccessNode {
    pub fn new<S: Into<Span>>(span: S, name: Token) -> Self {
        Self {
            span_: span.into(),
            name,
        }
    }
}

impl HasSpan for SubsequentDotAccessNode {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_subsequent_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
    greedy_after_block: bool,
) -> Option<ExpressionNode> {
    let left = parse_atom_expression(iter, errors)?;

    let follows = parse_follows(iter, errors);

    if follows.is_empty() {
        Some(left)
    } else {
        Some(ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            left.span() + follows.last().map(|x| x.span()).unwrap_or(Span::empty()),
            Box::new(left),
            follows,
        )))
    }
}

fn parse_follows<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Vec<SubsequentExpressionFollowNode> {
    let mut result = vec![];

    loop {
        if let Some(group) = consume_group(iter, OpenParen) {
            result.push(SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::call(group.span(), parse_args(group, errors)),
            ));
        } else if let Some(group) = consume_group(iter, OpenSquare) {
            result.push(SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::index(group.span(), parse_args(group, errors)),
            ));
        } else if let Some(with) = consume_token!(iter, With) {
            if let Some(group) = consume_group(iter, OpenParen) {
                result.push(SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::with(
                        with.span() + group.span(),
                        parse_args(group, errors),
                    ),
                ));
            } else {
                add_error!(errors, with.span().end(), MissingWithValues);
            }
        } else if let Some(dot) = consume_token!(iter, Dot) {
            if let Some(name) = consume_token!(iter, Identifier) {
                result.push(SubsequentExpressionFollowNode::DotAccess(
                    SubsequentDotAccessNode::new(dot.span() + name.span(), name),
                ));
            } else {
                add_error!(errors, dot.span().end(), MissingDotAccessName);
            }
        } else {
            break;
        };
    }

    result
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::expression_node::ExpressionNode::Subsequent;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::tokenizer::TokenType::{
        Comma, DecInteger, Dot, Equals, Identifier, Plus, Unknown, With,
    };
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use crate::parser::block_expression_node::BlockExpressionNode;

    #[test]
    fn empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
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
    fn just_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn single_call() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, Identifier:5 ):6, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 7),
                Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                )))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(4..7, vec![
                        ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:5)))))
                    ])
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn multiple_calls() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, ):6, (:8, ):10, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 11),
                Box::new(ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                )))),
                vec![
                    SubsequentExpressionFollowNode::CallLike(SubsequentCallLikeNode::call(
                        4..7,
                        vec![],
                    )),
                    SubsequentExpressionFollowNode::CallLike(SubsequentCallLikeNode::call(
                        8..11,
                        vec![]
                    )),
                ],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn with_expr_with_args() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, With:8..12, (:13, Identifier:15..20, Equals:22, DecInteger:23..25, Comma:26, DecInteger:30..32, Comma:36, ):40);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 41),
                Box::new(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6)))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::with(
                        8..41,
                        vec![
                            ArgNode::Named(NamedArgNode::new(
                                15..25,
                                test_token!(Identifier:15..20),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        23..25,
                                        test_token!(DecInteger:23..25),
                                        false,
                                    )
                                )))
                            )),
                            ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Literal(
                                LiteralExpressionNode::Number(NumberLiteralNode::new(
                                    30..32,
                                    test_token!(DecInteger:30..32),
                                    false,
                                ))
                            ))),
                        ],
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn index_expr_with_args() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, [:13, Identifier:15..20, Equals:21, DecInteger:23..25, Comma:26, DecInteger:30..32, Comma:36, ]:40);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 41),
                Box::new(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6)))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::index(
                        13..41,
                        vec![
                            ArgNode::Named(NamedArgNode::new(
                                15..25,
                                test_token!(Identifier:15..20),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        23..25,
                                        test_token!(DecInteger:23..25),
                                        false,
                                    )
                                )))
                            )),
                            ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Literal(
                                LiteralExpressionNode::Number(NumberLiteralNode::new(
                                    30..32,
                                    test_token!(DecInteger:30..32),
                                    false,
                                ))
                            ))),
                        ],
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn dot_access() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, Dot:7, Identifier:8..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Subsequent(SubsequentExpressionNode {
                span_: Span(1, 10),
                left: Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier:1..6)
                ))),
                follows: vec![SubsequentExpressionFollowNode::DotAccess(
                    SubsequentDotAccessNode::new(7..10, test_token!(Identifier:8..10))
                )],
            }))
        );
        errors.assert_empty();
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn dot_access_missing_name() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, Dot:7, Unknown:8..15);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:1..6)
            )))
        );
        assert!(errors.has_error_at(8, ErrorKind::MissingDotAccessName));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(
            remaining,
            test_tokentree!(Unknown:8..15).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn with_missing_arg_group() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, With:7..11, Unknown:15..20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:1..6)
            )))
        );
        assert!(errors.has_error_at(11, ErrorKind::MissingWithValues));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(
            remaining,
            test_tokentree!(Unknown:15..20).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn multiple_follows_with_missing_inbetween() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Identifier:1..6, With:7..11, [:12,]:13, Dot:14, (:15,):16);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Subsequent(SubsequentExpressionNode {
                span_: Span(1, 17),
                left: Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier:1..6)
                ))),
                follows: vec![
                    SubsequentExpressionFollowNode::CallLike(SubsequentCallLikeNode::index(
                        12..14,
                        vec![]
                    )),
                    SubsequentExpressionFollowNode::CallLike(SubsequentCallLikeNode::call(
                        15..17,
                        vec![]
                    )),
                ],
            }))
        );
        assert!(errors.has_error_at(11, ErrorKind::MissingWithValues));
        assert!(errors.has_error_at(15, ErrorKind::MissingDotAccessName));
        assert_eq!(errors.get_errors().len(), 2);
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }
}
