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
    DotAccess(SubsequentDotAccessNode),
}

impl HasSpan for SubsequentExpressionFollowNode {
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
pub enum ArgNode {
    Named(NamedArgNode),
    Unnamed(UnnamedArgNode),
}

impl HasSpan for ArgNode {
    fn span(&self) -> Span {
        match self {
            ArgNode::Named(x) => x.span(),
            ArgNode::Unnamed(x) => x.span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedArgNode {
    span_: Span,
    pub name: Token,
    pub value: Option<Box<ExpressionNode>>,
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
pub struct UnnamedArgNode {
    pub value: Box<ExpressionNode>,
}

impl HasSpan for UnnamedArgNode {
    fn span(&self) -> Span {
        self.value.span()
    }
}

impl UnnamedArgNode {
    pub fn new(expr: ExpressionNode) -> Self {
        Self {
            value: Box::new(expr),
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
                todo!("add error")
            }
        } else if let Some(dot) = consume_token!(iter, Dot) {
            if let Some(name) = consume_token!(iter, Identifier) {
                result.push(SubsequentExpressionFollowNode::DotAccess(SubsequentDotAccessNode::new(dot.span() + name.span(), name)));
            } else {
                todo!("add error")
            }
        } else {
            break;
        };
    }

    result
}

fn parse_args(group: &Group, errors: &mut Errors) -> Vec<ArgNode> {
    let mut args: Vec<ArgNode> = vec![];

    let mut iter = make_awesome(group.children.iter());

    loop {
        if let Some(arg) = parse_named_arg(&mut iter, errors)
            .or_else(|| parse_unnamed_arg(&mut iter, errors))
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

fn parse_unnamed_arg<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ArgNode> {
    if let Some(expr) = parse_expression(iter, errors, false) {
        Some(ArgNode::Unnamed(
            UnnamedArgNode {
                value: Box::new(expr),
            },
        ))
    } else {
        None
    }
}

fn parse_named_arg<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ArgNode> {
    let name = consume_token!(iter, Identifier)?;
    let equals_token = consume_token!(iter, Equals)?;
    let expr = parse_expression(iter, errors, false);

    Some(ArgNode::Named(NamedArgNode::new(
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
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::expression_node::ExpressionNode::Subsequent;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::tokenizer::TokenType::{Comma, DecInteger, Dot, Equals, Identifier, True, Unknown, With};
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn call_with_unnamed() {
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
                        vec![ArgNode::Unnamed(
                            UnnamedArgNode::new(ExpressionNode::Literal(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn call_with_unnamed_trailing_comma() {
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
                        vec![ArgNode::Unnamed(
                            UnnamedArgNode::new(ExpressionNode::Literal(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn call_with_multiple_unnamed() {
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
                            ArgNode::Unnamed(UnnamedArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        6..8,
                                        test_token!(DecInteger:6..8),
                                        false,
                                    )
                                ))
                            )),
                            ArgNode::Unnamed(UnnamedArgNode::new(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
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
                            ArgNode::Unnamed(UnnamedArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        6..8,
                                        test_token!(DecInteger:6..8),
                                        false,
                                    )
                                ))
                            )),
                            ArgNode::Unnamed(UnnamedArgNode::new(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
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
                        vec![ArgNode::Unnamed(
                            UnnamedArgNode::new(ExpressionNode::Literal(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
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
                        vec![ArgNode::Named(NamedArgNode::new(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
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
                            ArgNode::Named(NamedArgNode::new(
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
                            ArgNode::Named(NamedArgNode::new(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn call_with_multiple_named_and_unnamed() {
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
                            ArgNode::Named(NamedArgNode::new(
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
                            ArgNode::Unnamed(UnnamedArgNode::new(
                                ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        20..22,
                                        test_token!(DecInteger:20..22),
                                        false,
                                    )
                                ))
                            )),
                            ArgNode::Named(NamedArgNode::new(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
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
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6))),
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
                            ArgNode::Unnamed(UnnamedArgNode::new(
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
        assert_eq!(remaining, Vec::<&TokenTree>::new());
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
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6))),
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
                            ArgNode::Unnamed(UnnamedArgNode::new(
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
        assert_eq!(result, Some(ExpressionNode::Subsequent(SubsequentExpressionNode {
            span_: Span(1, 10),
            left: Box::new(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6)))),
            follows: vec![
                SubsequentExpressionFollowNode::DotAccess(SubsequentDotAccessNode::new(7..10, test_token!(Identifier:8..10)))
            ],
        })));
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
        todo!("determine which errors we want here");
        assert_eq!(result, Some(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6)))));
        assert!(errors.has_error_at(7, ErrorKind::MissingOperand));
        assert_eq!(remaining, test_tokentree!(Unknown:8..15).iter().collect::<Vec<_>>());
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
        todo!("determine which errors we want here");
        assert_eq!(result, Some(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6)))));
        assert!(errors.has_error_at(11, ErrorKind::MissingOperand));
        assert_eq!(remaining, test_tokentree!(Unknown:15..20).iter().collect::<Vec<_>>());
    }

    #[test]
    fn multiple_follows_with_missing_inbetween() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:1..6, With:7..11, [:12,]:13, Dot:14, (:15,):16);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_subsequent_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();


        // assert
        assert_eq!(result, Some(ExpressionNode::Subsequent(SubsequentExpressionNode {
            span_: Span(1, 17),
            left: Box::new(ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:1..6)))),
            follows: vec![
                SubsequentExpressionFollowNode::CallLike(SubsequentCallLikeNode::index(12..14, vec![])),
                SubsequentExpressionFollowNode::CallLike(SubsequentCallLikeNode::call(15..17, vec![])),
            ],
        })));
        todo!("determine which errors we want here");
        assert!(errors.has_error_at(11, ErrorKind::MissingOperand));
        assert!(errors.has_error_at(15, ErrorKind::MissingOperand));
        assert_eq!(remaining, Vec::<&TokenTree>::new());
    }

    #[test]
    fn named_arg_missing_value() {
        todo!("write this test")
    }
}
