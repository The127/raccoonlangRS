use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::Errors;
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::parser::recover_until;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::{Group, TokenTree};
use crate::{add_error, consume_token, token_starter};

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

pub fn parse_args(group: &Group, errors: &mut Errors) -> Vec<ArgNode> {
    let mut args: Vec<ArgNode> = vec![];

    let mut iter = make_awesome(group.children.iter());

    loop {
        if let Some(arg) =
            parse_named_arg(&mut iter, errors).or_else(|| parse_unnamed_arg(&mut iter, errors))
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

pub fn parse_unnamed_arg<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ArgNode> {
    if let Some(expr) = parse_expression(iter, errors, true) {
        Some(ArgNode::Unnamed(UnnamedArgNode::new(expr)))
    } else {
        None
    }
}

pub fn parse_named_arg<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ArgNode> {
    let name = consume_token!(iter, Identifier)?;
    let equals_token = consume_token!(iter, Equals)?;
    let expr = parse_expression(iter, errors, true);
    if expr.is_none() {
        add_error!(errors, equals_token.span().end(), MissingArgumentValue);
    }

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
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::{Group, TokenTree};
    use crate::{test_token, test_tokentree};

    fn make_group(children: Vec<TokenTree>) -> Group {
        Group {
            open: test_token!(OpenParen),
            children: children,
            close: Some(test_token!(CloseParen)),
        }
    }

    #[test]
    fn empty() {
        // arrange
        let group = make_group(vec![]);
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(result, vec![],);
        errors.assert_empty();
    }

    fn make_unnamed<S: Into<Span>>(span: S) -> ArgNode {
        let span = span.into();
        ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Access(
            AccessExpressionNode::new(test_token!(Identifier:span)),
        )))
    }

    fn make_named<S: Into<Span>>(name: S, value: S) -> ArgNode {
        let name = name.into();
        let value = value.into();
        ArgNode::Named(NamedArgNode::new(
            name + value,
            test_token!(Identifier:name),
            Some(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:value),
            ))),
        ))
    }

    #[test]
    fn single_unnamed() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:1));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(result, vec![make_unnamed(1),]);
        errors.assert_empty();
    }

    #[test]
    fn single_named() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:1, Equals:2, Identifier:3));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(result, vec![make_named(1, 3),]);
        errors.assert_empty();
    }

    #[test]
    fn multiple_mixed() {
        // arrange
        let group = make_group(test_tokentree!(
            Identifier:1, Equals:2, Identifier:3, Comma:4,
            Identifier:5, Comma:6,
            Identifier:7, Equals:8, Identifier:9, Comma:10,
            Identifier:11, Equals:12, Identifier:13, Comma:14,
            Identifier:15, Comma:16,
            Identifier:17, Comma:18,
            Identifier:19
        ));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_named(1, 3),
                make_unnamed(5),
                make_named(7, 9),
                make_named(11, 13),
                make_unnamed(15),
                make_unnamed(17),
                make_unnamed(19),
            ]
        );
        errors.assert_empty();
    }

    #[test]
    fn trailing_comma() {
        // arrange
        let group = make_group(
            test_tokentree!(Identifier:1, Equals:2, Identifier:3, Comma:4, Identifier:5, Comma:6),
        );
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(result, vec![make_named(1, 3), make_unnamed(5),]);
        errors.assert_empty();
    }

    #[test]
    fn repeated_comma() {
        // arrange
        let group = make_group(
            test_tokentree!(Identifier:1, Equals:2, Identifier:3, Comma:4, Comma:5, Comma:6, Identifier:7),
        );
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(result, vec![make_named(1, 3), make_unnamed(7),]);
        errors.assert_count(2);
        assert!(errors.has_error_at(5, ErrorKind::UnexpectedToken(Comma)));
        assert!(errors.has_error_at(6, ErrorKind::UnexpectedToken(Comma)));
    }

    #[test]
    fn missing_comma() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:1, Equals:2, Identifier:3, Identifier:5));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(result, vec![make_named(1, 3), make_unnamed(5),]);
        errors.assert_count(1);
        assert!(errors.has_error_at(4, ErrorKind::MissingComma));
    }

    #[test]
    fn missing_named_value() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:1, Equals:2, Comma:4, Identifier:5));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                ArgNode::Named(NamedArgNode::new(1..3, test_token!(Identifier:1), None)),
                make_unnamed(5),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(3, ErrorKind::MissingArgumentValue));
    }

    #[test]
    fn greedy_after_block_unnamed() {
        // arrange
        let group = make_group(test_tokentree!({ Identifier }, Plus, Identifier));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Add(
                AddExpressionNode::new(
                    Span::empty(),
                    ExpressionNode::Block(BlockExpressionNode::new(
                        Span::empty(),
                        vec![],
                        Some(
                            ExpressionNode::Access(AccessExpressionNode::new(test_token!(
                                Identifier
                            )))
                            .into()
                        )
                    ))
                    .into(),
                    vec![AddExpressionNodeFollow {
                        operator: test_token!(Plus),
                        operand: Some(ExpressionNode::Access(AccessExpressionNode::new(
                            test_token!(Identifier)
                        ))),
                    }]
                )
            )))]
        );
        errors.assert_empty();
    }

    #[test]
    fn greedy_after_block_named() {
        // arrange
        let group = make_group(test_tokentree!(
            Identifier,
            Equals,
            { Identifier },
            Plus,
            Identifier
        ));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![ArgNode::Named(NamedArgNode::new(
                Span::empty(),
                test_token!(Identifier),
                Some(ExpressionNode::Add(AddExpressionNode::new(
                    Span::empty(),
                    ExpressionNode::Block(BlockExpressionNode::new(
                        Span::empty(),
                        vec![],
                        Some(
                            ExpressionNode::Access(AccessExpressionNode::new(test_token!(
                                Identifier
                            )))
                            .into()
                        )
                    ))
                    .into(),
                    vec![AddExpressionNodeFollow {
                        operator: test_token!(Plus),
                        operand: Some(ExpressionNode::Access(AccessExpressionNode::new(
                            test_token!(Identifier)
                        ))),
                    }]
                )))
            ))]
        );
        errors.assert_empty();
    }

    #[test]
    fn unexpected_token_at_beginning() {
        // arrange
        let group = make_group(test_tokentree!(Unknown:1, Identifier:2, Comma:3, Identifier:4));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_unnamed(2),
                make_unnamed(4),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(1, ErrorKind::UnexpectedToken(Unknown)));
    }

    #[test]
    fn unexpected_token_at_end_no_comma() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:2, Comma:3, Identifier:4, Unknown:5));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_unnamed(2),
                make_unnamed(4),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(5, ErrorKind::UnexpectedToken(Unknown)));
    }

    #[test]
    fn unexpected_token_at_end_after_comma() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:2, Comma:3, Identifier:4, Comma:5, Unknown:6));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_unnamed(2),
                make_unnamed(4),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(6, ErrorKind::UnexpectedToken(Unknown)));
    }

    #[test]
    fn unexpected_token_after_comma_before_arg() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:2, Comma:3, Unknown:4, Identifier:5));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_unnamed(2),
                make_unnamed(5),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(4, ErrorKind::UnexpectedToken(Unknown)));
    }

    #[test]
    fn unexpected_token_after_arg_before_comma() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:2, Unknown:3, Comma:4, Identifier:5));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_unnamed(2),
                make_unnamed(5),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(3, ErrorKind::UnexpectedToken(Unknown)));
    }

    #[test]
    fn unexpected_token_between_comma() {
        // arrange
        let group = make_group(test_tokentree!(Identifier:1, Comma:2, Unknown:3, Comma:4, Identifier:5));
        let mut errors = Errors::new();

        // act
        let result = parse_args(&group, &mut errors);

        // assert
        assert_eq!(
            result,
            vec![
                make_unnamed(1),
                make_unnamed(5),
            ]
        );
        errors.assert_count(1);
        assert!(errors.has_error_at(3, ErrorKind::UnexpectedToken(Unknown)));
    }
}
