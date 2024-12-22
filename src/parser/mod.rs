mod file_node;
mod mod_node;
mod path_node;
mod use_node;

use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::MarkingIterator;
use crate::source_map::Span;
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::{Group, TokenTree};

#[derive(Default)]
struct Spanned<T> {
    span: Span,
    value: T,
}

type RecoverMatcher<'a, I>
where
    I: Iterator<Item = &'a TokenTree>,
= fn(iter: &mut dyn MarkingIterator<I>) -> bool;

fn recover_until<'a, const match_count: usize, const error_count: usize, I>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
    matchers: [RecoverMatcher<'a, I>; match_count],
    error_cases: [RecoverMatcher<'a, I>; error_count],
) -> bool
where
    I: Iterator<Item = &'a TokenTree>,
{
    let mut iter = iter.mark().auto_reset();
    let mut errored_already = false;
    loop {
        if matchers.iter().any(|f| f(&mut iter)) {
            return true;
        }
        if error_cases.iter().any(|f| f(&mut iter)) {
            return false;
        }
        match iter.next() {
            Some(TokenTree::Token(unexpected)) if !errored_already => {
                errors.add(
                    ErrorKind::UnexpectedToken(unexpected.token_type),
                    unexpected.span,
                );
                errored_already = true;
            }
            Some(TokenTree::Group(unexpected)) if !errored_already => {
                errors.add(
                    ErrorKind::UnexpectedToken(unexpected.open.token_type),
                    unexpected.open.span,
                );
                errored_already = true;
            }
            None => return false,
            _ => (),
        }
    }
}

#[macro_export]
macro_rules! token_starter {
    ($name:ident, $token_type:ident) => {
        fn $name<'a, I: Iterator<Item = &'a TokenTree>>(iter: &mut dyn MarkingIterator<I>) -> bool {
            let mut mark = iter.mark().auto_reset();
            let result = match (mark.next()) {
                Some(TokenTree::Token(Token {
                    token_type: $token_type,
                    ..
                })) => true,
                _ => false,
            };

            result
        }
    };
}

#[macro_export]
macro_rules! tt_starter {
    ($name:ident, $token_type:ident) => {
        fn $name<'a, I: Iterator<Item = &'a TokenTree>>(iter: &mut dyn MarkingIterator<I>) -> bool {
            let mut mark = iter.mark().auto_reset();
            let result = match (mark.next()) {
                Some(TokenTree::Group(Group {
                    open:
                        Token {
                            token_type: $token_type,
                            ..
                        },
                    ..
                })) => true,
                _ => false,
            };

            result
        }
    };
}

pub fn expect_token<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut I,
    token_type: TokenType,
) -> Token {
    match iter.next() {
        Some(TokenTree::Token(token)) if token.token_type == token_type => token.clone(),
        Some(TokenTree::Token(token)) => {
            panic!("expected: {:?}, found: {:?}", token_type, token.token_type)
        }
        Some(TokenTree::Group(group)) => panic!(
            "expected: {:?}, found: {:?}",
            token_type, group.open.token_type
        ),
        _ => panic!("expected: {:?}, found: None", token_type),
    }
}

//TODO: this wants tests
pub fn consume_group<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    token_type: TokenType,
) -> Option<&'a Group> {
    let mut mark = iter.mark();
    if let Some(TokenTree::Group(group)) = mark.next() {
        if group.open.token_type == token_type {
            return Some(group);
        }
    }

    // TODO: error if the group is not closed

    mark.reset();
    None
}

pub fn consume_token<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    token_type: TokenType,
) -> Option<Token> {
    let mut mark = iter.mark();
    if let Some(TokenTree::Token(token)) = mark.next() {
        if token.token_type == token_type {
            return Some(*token);
        }
    }
    mark.reset();
    None
}

//TODO: this wants tests
fn consume_tokens<'a, const count: usize, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    types: [TokenType; count],
) -> Option<[Token; count]> {
    let mut mark = iter.mark();

    let mut tokens: [Token; count] = unsafe { std::mem::zeroed() };

    for i in 0..types.len() {
        let token_type = types[i];
        match mark.next() {
            Some(TokenTree::Token(token)) if token.token_type == token_type => {
                tokens[i] = *token;
            }
            _ => {
                mark.reset();
                return None;
            }
        }
    }

    return Some(tokens);
}

mod test_utils {

    #[macro_export]
    macro_rules! test_token {
        ($name:ident) => {
            Token {
                token_type: $name,
                span: Span::empty(),
            }
        };
    }

    #[macro_export]
    macro_rules! test_tokens {
        ($($name:ident)*) => {
            vec![
                $(
                    Token {
                        token_type: $name,
                        span: Span::empty(),
                    }
                ),*
            ]
        };
    }

    #[macro_export]
    macro_rules! test_tokentree_helper {
        ($token_type:ident) => {
            TokenTree::Token(test_token!($token_type))
        };
        ({$($token_type:tt)*}) => {
            TokenTree::Group(Group {
                open: test_token!(OpenCurly),
                close: Some(test_token!(CloseCurly)),
                children: test_tokentree!($($token_type)*),
            })
        };
        (($($token_type:tt)*)) => {
            TokenTree::Group(Group {
                open: test_token!(OpenParen),
                close: Some(test_token!(CloseParen)),
                children: test_tokentree!($($token_type)*),
            })
        };
        ([$($token_type:tt)*]) => {
            TokenTree::Group(Group {
                open: test_token!(OpenSquare),
                close: Some(test_token!(CloseSquare)),
                children: test_tokentree!($($token_type)*),
            })
        };
        (<$($token_type:tt)*>) => {
            TokenTree::Group(Group {
                open: test_token!(OpenAngle),
                close: Some(test_token!(CloseAngle)),
                children: test_tokentree!($($token_type)*),
            })
        };
    }

    #[macro_export]
    macro_rules! test_tokentree {
        ($($input:tt)*) => {
            vec![$(
                test_tokentree_helper!($input)
            ),*]
        };
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Error;
    use crate::marking_iterator::marking;
    use crate::source_map::Span;
    use crate::tokenizer::Token;
    use crate::tokenizer::TokenType::*;
    use crate::{test_token, test_tokentree, test_tokentree_helper};

    #[test]
    fn expect_is_found() {
        // arrange
        let input = test_tokentree!(Mod);
        let mut iter = input.iter();

        // act
        expect_token(&mut iter, Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];
        let mut iter = input.iter();

        // act
        expect_token(&mut iter, Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_not_found() {
        // arrange
        let input = test_tokentree!(Identifier);
        let mut iter = input.iter();

        // act
        expect_token(&mut iter, Mod);
    }

    #[test]
    fn recover_finds_matching_immediate() {
        // arrange
        let input = test_tokentree!(Identifier Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), input.iter().collect::<Vec<_>>());
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn recover_finds_matching_not_immediate() {
        // arrange
        let input = test_tokentree!(Unknown Identifier Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);
        token_starter!(semicolon, Semicolon);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], [semicolon]);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), input.iter().collect::<Vec<_>>());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                location: Span::empty(),
            }]
        );
    }

    #[test]
    fn recover_finds_error_cases_not_immediate() {
        // arrange
        let input = test_tokentree!(Unknown Identifier Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);
        token_starter!(semicolon, Semicolon);

        // act
        let found = recover_until(&mut iter, &mut errors, [semicolon], [identifier]);

        // assert
        assert_eq!(found, false);
        assert_eq!(iter.collect::<Vec<_>>(), input.iter().collect::<Vec<_>>());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                location: Span::empty(),
            }]
        );
    }

    #[test]
    fn recover_only_one_unexpected_error() {
        // arrange
        let input = test_tokentree!(Unknown Semicolon Identifier);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), input.iter().collect::<Vec<_>>());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                location: Span::empty(),
            }]
        );
    }
}
