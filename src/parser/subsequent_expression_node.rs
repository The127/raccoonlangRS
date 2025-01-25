use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::Errors;
use crate::parser::consume_group;
use crate::parser::expression_node::{parse_atom_expression, parse_expression, ExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::OpenParen;
use crate::treeizer::{Group, TokenTree};
use crate::{consume_token, expect_token};

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
    Call(SubsequentCallNode),
}

impl HasSpan for SubsequentExpressionFollowNode {
    fn span(&self) -> Span {
        match self {
            SubsequentExpressionFollowNode::Call(x) => x.span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentCallNode {
    span_: Span,
    params: Vec<SubsequentCallParamNode>,
}

impl HasSpan for SubsequentCallNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl SubsequentCallNode {
    pub fn new<S: Into<Span>>(span: S, params: Vec<SubsequentCallParamNode>) -> Self {
        Self {
            span_: span.into(),
            params,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SubsequentCallParamNode {
    Named(SubsequentCallParamNamedNode),
    Positional(SubsequentCallParamPositionalNode),
}

impl HasSpan for SubsequentCallParamNode {
    fn span(&self) -> Span {
        match self {
            SubsequentCallParamNode::Named(x) => x.span(),
            SubsequentCallParamNode::Positional(x) => x.span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentCallParamNamedNode {
    span_: Span,
    name: Token,
    value: Option<Box<ExpressionNode>>,
}

impl HasSpan for SubsequentCallParamNamedNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl SubsequentCallParamNamedNode {
    pub fn new<S: Into<Span>>(span: S, name: Token, value: Option<ExpressionNode>) -> Self {
        Self {
            span_: span.into(),
            name,
            value: value.map(Box::new),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SubsequentCallParamPositionalNode {
    expr: Box<ExpressionNode>,
}

impl HasSpan for SubsequentCallParamPositionalNode {
    fn span(&self) -> Span {
        self.expr.span()
    }
}

impl SubsequentCallParamPositionalNode {
    pub fn new(expr: ExpressionNode) -> Self {
        Self {
            expr: Box::new(expr),
        }
    }
}

pub fn parse_subsequent_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _errors: &mut Errors,
    greedy_after_block: bool,
) -> Option<ExpressionNode> {
    let left = parse_atom_expression(iter, _errors)?;

    let follows = parse_follows(iter, _errors);

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
    _errors: &mut Errors,
) -> Vec<SubsequentExpressionFollowNode> {
    let mut result = vec![];

    loop {
        if let Some(group) = consume_group(iter, OpenParen) {
            result.push(parse_call(group, _errors));
            continue;
        }

        break;
    }

    result
}

fn parse_call(group: &Group, _errors: &mut Errors) -> SubsequentExpressionFollowNode {
    let mut params = vec![];

    let mut iter = make_awesome(group.children.iter());

    loop {
        if let Some(param) = parse_call_named_param(&mut iter, _errors)
            .or_else(|| parse_call_positional_param(&mut iter, _errors))
        {
            params.push(param);

            consume_token!(iter, Comma);
        } else {
            break;
        }
    }

    SubsequentExpressionFollowNode::Call(SubsequentCallNode {
        span_: group.span(),
        params,
    })
}

fn parse_call_positional_param<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _errors: &mut Errors,
) -> Option<SubsequentCallParamNode> {
    if let Some(expr) = parse_expression(iter, _errors, false) {
        Some(SubsequentCallParamNode::Positional(
            SubsequentCallParamPositionalNode {
                expr: Box::new(expr),
            },
        ))
    } else {
        None
    }
}

fn parse_call_named_param<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _errors: &mut Errors,
) -> Option<SubsequentCallParamNode> {
    let name = consume_token!(iter, Identifier)?;
    let equals_token = consume_token!(iter, Equals)?;
    let expr = parse_expression(iter, _errors, false);

    Some(SubsequentCallParamNode::Named(
        SubsequentCallParamNamedNode::new(
            name.span()
                + equals_token.span()
                + expr.as_ref().map(|x| x.span()).unwrap_or(Span::empty()),
            name,
            expr,
        ),
    ))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::parser::expression_node::ExpressionNode::Subsequent;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::tokenizer::TokenType::{Comma, DecInteger, Equals, Identifier, Unknown};
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(4..7, vec![])
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
                    SubsequentExpressionFollowNode::Call(SubsequentCallNode::new(4..7, vec![],)),
                    SubsequentExpressionFollowNode::Call(SubsequentCallNode::new(8..11, vec![],)),
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(
                        4..17,
                        vec![SubsequentCallParamNode::Positional(
                            SubsequentCallParamPositionalNode::new(ExpressionNode::Literal(
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(
                        4..17,
                        vec![SubsequentCallParamNode::Positional(
                            SubsequentCallParamPositionalNode::new(ExpressionNode::Literal(
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(
                        4..17,
                        vec![
                            SubsequentCallParamNode::Positional(
                                SubsequentCallParamPositionalNode::new(ExpressionNode::Literal(
                                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                                        6..8,
                                        test_token!(DecInteger:6..8),
                                        false,
                                    ))
                                ))
                            ),
                            SubsequentCallParamNode::Positional(
                                SubsequentCallParamPositionalNode::new(ExpressionNode::Literal(
                                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                                        11..14,
                                        test_token!(DecInteger:11..14),
                                        false,
                                    ))
                                ))
                            )
                        ]
                    )
                )],
            ))),
        );
        errors.assert_empty();
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(
                        4..17,
                        vec![SubsequentCallParamNode::Named(
                            SubsequentCallParamNamedNode::new(
                                6..15,
                                test_token!(Identifier:6..10),
                                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                    NumberLiteralNode::new(
                                        13..15,
                                        test_token!(DecInteger:13..15),
                                        false,
                                    )
                                )))
                            )
                        ),]
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(
                        4..27,
                        vec![
                            SubsequentCallParamNode::Named(SubsequentCallParamNamedNode::new(
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
                            SubsequentCallParamNode::Named(SubsequentCallParamNamedNode::new(
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
                vec![SubsequentExpressionFollowNode::Call(
                    SubsequentCallNode::new(
                        4..37,
                        vec![
                            SubsequentCallParamNode::Named(SubsequentCallParamNamedNode::new(
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
                            SubsequentCallParamNode::Positional(
                                SubsequentCallParamPositionalNode::new(ExpressionNode::Literal(
                                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                                        20..22,
                                        test_token!(DecInteger:20..22),
                                        false,
                                    ))
                                ))
                            ),
                            SubsequentCallParamNode::Named(SubsequentCallParamNamedNode::new(
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
}
