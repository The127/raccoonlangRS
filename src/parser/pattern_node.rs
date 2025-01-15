use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::{ErrorKind, Errors};
use crate::parser::{consume_group, recover_until, Spanned};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::{Group, TokenTree};
use crate::{consume_token, token_starter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PatternNode {
    Discard(Token),
    Name(Token),
    Tuple(Spanned<Vec<PatternNode>>),
}

impl HasSpan for PatternNode {
    fn span(&self) -> Span {
        match self {
            PatternNode::Discard(x) => x.span(),
            PatternNode::Name(x) => x.span(),
            PatternNode::Tuple(x) => x.span(),
        }
    }
}

pub fn pattern_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    match iter.peek() {
        Some(TokenTree::Token(Token {
            token_type: TokenType::Identifier|TokenType::Discard,
            ..
        })) => true,
        Some(TokenTree::Group(Group {
            open:
                Token {
                    token_type: TokenType::OpenParen,
                    ..
                },
            ..
        })) => true,
        _ => false,
    }
}

pub fn parse_pattern<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<PatternNode> {
    token_starter!(comma, Comma);

    if let Some(token) = consume_token!(iter, Identifier) {
        Some(PatternNode::Name(token))
    } else if let Some(token) = consume_token!(iter, Discard) {
        Some(PatternNode::Discard(token))
    } else if let Some(group) = consume_group(iter, TokenType::OpenParen) {
        let mut iter = make_awesome(group.children.iter());
        let mut patterns = vec![];
        let mut missing_comma_at = None;
        loop {
            if let Some(pattern) = parse_pattern(&mut iter, errors) {
                if let Some(pos) = missing_comma_at {
                    errors.add(ErrorKind::MissingComma, pos);
                    missing_comma_at = None;
                }

                recover_until(&mut iter, errors, [comma, pattern_starter], []);
                if consume_token!(&mut iter, Comma).is_none() {
                    missing_comma_at = Some(pattern.span().end());
                }
                patterns.push(pattern);
            } else {
                break;
            }
        }
        Some(PatternNode::Tuple(Spanned::new(group.span(), patterns)))
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind::{MissingComma, UnexpectedToken};
    use crate::errors::Errors;
    use crate::parser::pattern_node::parse_pattern;
    use crate::tokenizer::TokenType::*;
    use crate::{test_token, test_tokens, test_tokentree};

    #[test]
    fn empty() {
        // arrange
        let tokens = test_tokentree!();
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn unknown_token() {
        // arrange
        let tokens = test_tokentree!(Unknown);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
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
    fn name() {
        // arrange
        let tokens = test_tokentree!(Identifier);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(PatternNode::Name(test_token!(Identifier))));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn discard() {
        // arrange
        let tokens = test_tokentree!(Discard);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(PatternNode::Discard(test_token!(Discard))));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn tuple() {
        // arrange
        let tokens = test_tokentree!((Identifier, Comma, Discard, Comma, Identifier));
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(PatternNode::Tuple(Spanned::new(
                Span::empty(),
                vec![
                    PatternNode::Name(test_token!(Identifier)),
                    PatternNode::Discard(test_token!(Discard)),
                    PatternNode::Name(test_token!(Identifier)),
                ]
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn nested_tuple() {
        // arrange
        let tokens = test_tokentree!((
            Identifier,
            Comma,
            (Identifier, Comma, Identifier),
            Comma,
            (Identifier, Comma, (Discard, Comma, Identifier))
        ));
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(PatternNode::Tuple(Spanned::new(
                Span::empty(),
                vec![
                    PatternNode::Name(test_token!(Identifier)),
                    PatternNode::Tuple(Spanned::new(
                        Span::empty(),
                        vec![
                            PatternNode::Name(test_token!(Identifier)),
                            PatternNode::Name(test_token!(Identifier)),
                        ]
                    )),
                    PatternNode::Tuple(Spanned::new(
                        Span::empty(),
                        vec![
                            PatternNode::Name(test_token!(Identifier)),
                            PatternNode::Tuple(Spanned::new(
                                Span::empty(),
                                vec![
                                    PatternNode::Discard(test_token!(Discard)),
                                    PatternNode::Name(test_token!(Identifier)),
                                ]
                            )),
                        ]
                    )),
                ]
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn tuple_trailing_comma() {
        // arrange
        let tokens = test_tokentree!((Identifier, Comma, Identifier, Comma));
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(PatternNode::Tuple(Spanned::new(
                Span::empty(),
                vec![
                    PatternNode::Name(test_token!(Identifier)),
                    PatternNode::Name(test_token!(Identifier)),
                ]
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn tuple_unexpected_token() {
        // arrange
        let tokens =
            test_tokentree!((:1, Identifier:2..5, Unknown:6..10, Comma:11, Identifier:12..15):16);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(PatternNode::Tuple(Spanned::new(
                (1..17),
                vec![
                    PatternNode::Name(test_token!(Identifier:2..5)),
                    PatternNode::Name(test_token!(Identifier:12..15)),
                ]
            )))
        );
        assert!(errors.has_error_at(6..10, UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn tuple_missing_comma() {
        // arrange
        let tokens = test_tokentree!((:1, Identifier:2..5, Discard:12..15, Identifier:17..20, (:22, Identifier:24..30, ):31, ):32);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_pattern(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(PatternNode::Tuple(Spanned::new(
                (1..33),
                vec![
                    PatternNode::Name(test_token!(Identifier:2..5)),
                    PatternNode::Discard(test_token!(Discard:12..15)),
                    PatternNode::Name(test_token!(Identifier:17..20)),
                    PatternNode::Tuple(Spanned::new(22..32, vec![
                        PatternNode::Name(test_token!(Identifier:24..30)),
                    ])),
                ]
            )))
        );
        assert!(errors.has_error_at(5, MissingComma));
        assert!(errors.has_error_at(15, MissingComma));
        assert!(errors.has_error_at(20, MissingComma));
        assert_eq!(errors.get_errors().len(), 3);
        assert!(remaining.is_empty());
    }
}
