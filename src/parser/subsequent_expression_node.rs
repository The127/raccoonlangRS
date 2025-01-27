use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::Errors;
use crate::parser::expression_node::{parse_atom_expression, parse_expression, ExpressionNode};
use crate::parser::{consume_group, consume_tokens, recover_until};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{Dot, OpenParen, OpenSquare, With};
use crate::treeizer::{Group, TokenTree};
use crate::{consume_token, token_starter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentExpressionNode {
    span_: Span,
    pub left: Box<ExpressionNode>,
    pub follows: Vec<SubsequentExpressionFollowNode>,
}

impl HasSpan for SubsequentExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl SubsequentExpressionNode {
    pub fn new<S: Into<Span>>(
        span: S,
        left: ExpressionNode,
        follows: Vec<SubsequentExpressionFollowNode>,
    ) -> Self {
        Self {
            span_: span.into(),
            left: Box::new(left),
            follows,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SubsequentExpressionFollowNode {
    CallLike(SubsequentCallLikeNode),
}

impl HasSpan for SubsequentExpressionFollowNode {
    fn span(&self) -> Span {
        match self {
            SubsequentExpressionFollowNode::CallLike(x) => x.span(),
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
    type_: CallLikeType,
    args: Vec<SubsequentArgNode>,
}

impl HasSpan for SubsequentCallLikeNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl SubsequentCallLikeNode {
    pub fn call<S: Into<Span>>(span: S, args: Vec<SubsequentArgNode>) -> Self {
        Self {
            span_: span.into(),
            type_: CallLikeType::Call,
            args,
        }
    }

    pub fn index<S: Into<Span>>(span: S, args: Vec<SubsequentArgNode>) -> Self {
        Self {
            span_: span.into(),
            type_: CallLikeType::Index,
            args,
        }
    }

    pub fn with<S: Into<Span>>(span: S, args: Vec<SubsequentArgNode>) -> Self {
        Self {
            span_: span.into(),
            type_: CallLikeType::With,
            args,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SubsequentArgNode {
    Named(NamedArgNode),
    Positional(PositionalArgNode),
}

impl HasSpan for SubsequentArgNode {
    fn span(&self) -> Span {
        match self {
            SubsequentArgNode::Named(x) => x.span(),
            SubsequentArgNode::Positional(x) => x.span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedArgNode {
    span_: Span,
    name: Token,
    value: Option<Box<ExpressionNode>>,
}

impl HasSpan for NamedArgNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl NamedArgNode {
    pub fn new<S: Into<Span>>(span: S, name: Token, value: Option<ExpressionNode>) -> Self {
        Self {
            span_: span.into(),
            name,
            value: value.map(Box::new),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PositionalArgNode {
    expr: Box<ExpressionNode>,
}

impl HasSpan for PositionalArgNode {
    fn span(&self) -> Span {
        self.expr.span()
    }
}

impl PositionalArgNode {
    pub fn new(expr: ExpressionNode) -> Self {
        Self {
            expr: Box::new(expr),
        }
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
            left,
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
                SubsequentCallLikeNode::call(
                    group.span(),
                    parse_args(group, errors),
                )));
        } else if let Some(group) = consume_group(iter, OpenSquare) {
            result.push(SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::index(
                    group.span(),
                    parse_args(group, errors),
                )
            ));
        } else if let Some(with) = consume_token!(iter, With) {
            if let Some(group) = consume_group(iter, OpenParen) {
                result.push(SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::with(
                        with.span() + group.span(),
                        parse_args(group, errors),
                    )
                ));
            } else {
                unreachable!()
            }
        } else {
            break;
        };
    }

    result
}

fn parse_args(group: &Group, errors: &mut Errors) -> Vec<SubsequentArgNode> {
    let mut args: Vec<SubsequentArgNode> = vec![];

    let mut iter = make_awesome(group.children.iter());

    loop {
        if let Some(arg) = parse_named_arg(&mut iter, errors)
            .or_else(|| parse_positional_arg(&mut iter, errors))
        {
            args.push(arg);

            token_starter!(comma, Comma);
            recover_until(&mut iter, errors, [comma], []);
            consume_token!(iter, Comma);
        } else {
            break;
        }
    }

    args
}

fn parse_positional_arg<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<SubsequentArgNode> {
    if let Some(expr) = parse_expression(iter, errors, false) {
        Some(SubsequentArgNode::Positional(
            PositionalArgNode {
                expr: Box::new(expr),
            },
        ))
    } else {
        None
    }
}

fn parse_named_arg<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<SubsequentArgNode> {
    let name = consume_token!(iter, Identifier)?;
    let equals_token = consume_token!(iter, Equals)?;
    let expr = parse_expression(iter, errors, false);

    Some(SubsequentArgNode::Named(NamedArgNode::new(
        name.span()
            + equals_token.span()
            + expr.as_ref().map(|x| x.span()).unwrap_or(Span::empty()),
        name,
        expr,
    )))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::errors::Errors;
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::expression_node::ExpressionNode::Subsequent;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::tokenizer::TokenType::{Comma, DecInteger, Dot, Equals, Identifier, Unknown, With};
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

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
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
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
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn empty_call() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, ):6, );
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
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(4..7, vec![])
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
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
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
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
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_positional() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, DecInteger:6..8, ):16, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 17),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..17,
                        vec![SubsequentArgNode::Positional(
                            PositionalArgNode::new(ExpressionNode::Literal(
                                LiteralExpressionNode::Number(NumberLiteralNode::new(
                                    6..8,
                                    test_token!(DecInteger:6..8),
                                    false,
                                ))
                            ))
                        )]
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_positional_trailing_comma() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(DecInteger:1..2, (:4, DecInteger:6..8, Comma:10, ):16, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 17),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..17,
                        vec![SubsequentArgNode::Positional(
                            PositionalArgNode::new(ExpressionNode::Literal(
                                LiteralExpressionNode::Number(NumberLiteralNode::new(
                                    6..8,
                                    test_token!(DecInteger:6..8),
                                    false,
                                ))
                            ))
                        )]
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_multiple_positional() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, DecInteger:6..8, Comma:10, DecInteger:11..14, ):16, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 17),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..17,
                        vec![
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        6..8,
                                        test_token!(DecInteger:6..8),
                                        false,
                                    )
                                ))
                            )),
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        11..14,
                                        test_token!(DecInteger:11..14),
                                        false,
                                    )
                                ))
                            ))
                        ]
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_multiple_unknown_before_comma() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, DecInteger:6..8, Unknown:9, Comma:10, DecInteger:11..14, ):16, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 17),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..17,
                        vec![
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        6..8,
                                        test_token!(DecInteger:6..8),
                                        false,
                                    )
                                ))
                            )),
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        11..14,
                                        test_token!(DecInteger:11..14),
                                        false,
                                    )
                                ))
                            ))
                        ]
                    )
                )],
            ))),
        );
        errors.has_error_at(9, UnexpectedToken(Unknown));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_multiple_comma_missing() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(DecInteger:1..2, (:4, DecInteger:6..8, DecInteger:11..14, ):16, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 17),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..17,
                        vec![SubsequentArgNode::Positional(
                            PositionalArgNode::new(ExpressionNode::Literal(
                                LiteralExpressionNode::Number(NumberLiteralNode::new(
                                    6..8,
                                    test_token!(DecInteger:6..8),
                                    false,
                                ))
                            ))
                        ),]
                    )
                )],
            ))),
        );
        errors.has_error_at(11, UnexpectedToken(DecInteger));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_named() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, Identifier:6..10, Equals:11, DecInteger:13..15 ):16, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 17),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..17,
                        vec![SubsequentArgNode::Named(NamedArgNode::new(
                            6..15,
                            test_token!(Identifier:6..10),
                            Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                NumberLiteralNode::new(
                                    13..15,
                                    test_token!(DecInteger:13..15),
                                    false,
                                )
                            )))
                        )),]
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_multiple_named() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, Identifier:6..10, Equals:11, DecInteger:13..15, Comma:16, Identifier:17..20, Equals:21, DecInteger:23..25 ):26, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 27),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..27,
                        vec![
                            SubsequentArgNode::Named(NamedArgNode::new(
                                6..15,
                                test_token!(Identifier:6..10),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        13..15,
                                        test_token!(DecInteger:13..15),
                                        false,
                                    )
                                )))
                            )),
                            SubsequentArgNode::Named(NamedArgNode::new(
                                17..25,
                                test_token!(Identifier:17..20),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        23..25,
                                        test_token!(DecInteger:23..25),
                                        false,
                                    )
                                )))
                            )),
                        ]
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn call_with_multiple_named_and_positional() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, (:4, Identifier:6..10, Equals:11, DecInteger:13..15, Comma:16, DecInteger:20..22, Comma:26, Identifier:27..30, Equals:31, DecInteger:33..35 ):36, );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 37),
                ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode::new(
                    1..2,
                    test_token!(DecInteger:1..2),
                    false
                ))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::call(
                        4..37,
                        vec![
                            SubsequentArgNode::Named(NamedArgNode::new(
                                6..15,
                                test_token!(Identifier:6..10),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        13..15,
                                        test_token!(DecInteger:13..15),
                                        false,
                                    )
                                )))
                            )),
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        20..22,
                                        test_token!(DecInteger:20..22),
                                        false,
                                    )
                                ))
                            )),
                            SubsequentArgNode::Named(NamedArgNode::new(
                                27..35,
                                test_token!(Identifier:27..30),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        33..35,
                                        test_token!(DecInteger:33..35),
                                        false,
                                    )
                                )))
                            )),
                        ]
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn with_expr_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, With:8..12, (:13, ):14);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 15),
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::with(8..15, vec![],)
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
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
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::with(
                        8..41,
                        vec![
                            SubsequentArgNode::Named(NamedArgNode::new(
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
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        30..32,
                                        test_token!(DecInteger:30..32),
                                        false,
                                    )
                                ))
                            )),
                        ],
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn index_expr_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, [:13, ]:14);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Subsequent(SubsequentExpressionNode::new(
                Span(1, 15),
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::index(13..15, vec![],)
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
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
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6))),
                vec![SubsequentExpressionFollowNode::CallLike(
                    SubsequentCallLikeNode::index(
                        13..41,
                        vec![
                            SubsequentArgNode::Named(NamedArgNode::new(
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
                            SubsequentArgNode::Positional(PositionalArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        30..32,
                                        test_token!(DecInteger:30..32),
                                        false,
                                    )
                                ))
                            )),
                        ],
                    )
                )],
            ))),
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
