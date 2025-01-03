mod file_node;
mod mod_node;
mod path_node;
mod use_node;
mod fn_node;
mod type_node;
mod return_type_node;
mod fn_parameters;
mod literal_expression;
mod expression_node;

use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::MarkingIterator;
use crate::source_map::Span;
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::{Group, TokenTree};

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub enum Visibility {
    #[default]
    Module,
    Public(Token),
}

#[derive(Default)]
struct Spanned<T> {
    span: Span,
    value: T,
}

type RecoverMatcher<'a, I>
where
    I: Iterator<Item = &'a TokenTree>,
= fn(iter: &mut dyn MarkingIterator<I>) -> bool;

/// Returns true if a matcher matches, false if the iterator ends or an error_case matches.
fn recover_until<'a, const match_count: usize, const error_count: usize, I>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
    matchers: [RecoverMatcher<'a, I>; match_count],
    error_cases: [RecoverMatcher<'a, I>; error_count],
) -> bool
where
    I: Iterator<Item = &'a TokenTree>,
{
    let mut errored_already = false;
    loop {
        if matchers.iter().any(|f| f(iter)) {
            return true;
        }
        if error_cases.iter().any(|f| f(iter)) {
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
        fn $name<'a, I: core::iter::Iterator<Item = &'a crate::treeizer::TokenTree>>(iter: &mut dyn crate::marking_iterator::MarkingIterator<I>) -> bool {
            let mut mark = iter.mark().auto_reset();
            let result = match (mark.next()) {
                Some(crate::treeizer::TokenTree::Token(crate::tokenizer::Token {
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
macro_rules! group_starter {
    ($name:ident, $token_type:ident) => {
        fn $name<'a, I: core::iter::Iterator<Item = &'a crate::treeizer::TokenTree>>(iter: &mut dyn crate::marking_iterator::MarkingIterator<I>) -> bool {
            let mut mark = iter.mark().auto_reset();
            let result = match (mark.next()) {
                Some(crate::treeizer::TokenTree::Group(crate::treeizer::Group {
                    open:
                        crate::tokenizer::Token {
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

    Some(tokens)
}

mod test_utils {

    #[macro_export]
    macro_rules! test_token {
        ($name:ident) => {
            crate::tokenizer::Token {
                token_type: $name,
                span: crate::source_map::Span::empty(),
            }
        };
        ($name:ident : $span:expr) => {
            crate::tokenizer::Token {
                token_type: $name,
                span: $span.into(),
            }
        };
    }

    #[macro_export]
    macro_rules! test_tokens {
        (@token $name:ident) => {
            crate::tokenizer::Token {
                token_type: $name,
                span: crate::source_map::Span::empty(),
            }
        };
        (@token $name:ident, $span:expr) => {
            crate::tokenizer::Token {
                token_type: $name,
                span: $span.into(),
            }
        };
        ($($name:ident $(:$span:expr)?),*) => {
            vec![
                $(
                    test_tokens!(@token $name $(, $span)?)
                ),*
            ]
        };
    }

    #[macro_export]
    macro_rules! test_tokentree {
        (@token $name:ident) => {
            crate::tokenizer::Token {
                token_type: $name,
                span: crate::source_map::Span::empty(),
            }
        };
        (@token $name:ident, $span:expr) => {
            crate::tokenizer::Token {
                token_type: $name,
                span: $span.into(),
            }
        };
        (@single $token_type:ident) => {
            crate::treeizer::TokenTree::Token(test_tokentree!(@token $token_type))
        };
        (@single $token_type:ident, $span:expr) => {
            crate::treeizer::TokenTree::Token(test_tokentree!(@token $token_type, $span))
        };
        (@single {$($tree:tt)*} $(, $close_span:expr)?) => {
            test_tokentree!(@group OpenCurly, CloseCurly, [$($tree)*] $(, $close_span)?)
        };
        (@single ($($tree:tt)*) $(, $close_span:expr)?) => {
            test_tokentree!(@group OpenParen, CloseParen, [$($tree)*] $(, $close_span)?)
        };
        (@single [$($tree:tt)*] $(, $close_span:expr)?) => {
            test_tokentree!(@group OpenSquare, CloseSquare, [$($tree)*] $(, $close_span)?)
        };
        (@single <$($tree:tt)*> $(, $close_span:expr)?) => {
            test_tokentree!(@group OpenAngle, CloseAngle, [$($tree)*] $(, $close_span)?)
        };
        (@group $opener:ident, $closer:ident, [:$open_span:expr, $($children:tt)*] $(, $close_span:expr)?) => {
            crate::treeizer::TokenTree::Group(crate::treeizer::Group {
                open: test_tokentree!(@token $opener, $open_span),
                close: Some(test_tokentree!(@token $closer $(, $close_span)?)),
                children: test_tokentree!($($children)*),
            })
        };
        (@group $opener:ident, $closer:ident, [$($children:tt)*] $(, $close_span:expr)?) => {
            crate::treeizer::TokenTree::Group(crate::treeizer::Group {
                open: test_tokentree!(@token $opener),
                close: Some(test_tokentree!(@token $closer $(, $close_span)?)),
                children: test_tokentree!($($children)*),
            })
        };
        ($($input:tt $(:$span:expr)?),*$(,)?) => {
            vec![$(
                test_tokentree!(@single $input $(, $span)?)
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
    use crate::{test_tokens, test_tokentree};

    #[test]
    fn test_tokentree_with_spans() {
        let input = test_tokentree!(Use, Identifier:2..3, Equals:5, Identifier);
        let expected = vec![
            TokenTree::Token(Token {
                token_type: Use,
                span: Span::empty(),
            }),
            TokenTree::Token(Token {
                token_type: Identifier,
                span: (2..3).into(),
            }),
            TokenTree::Token(Token {
                token_type: Equals,
                span: 5.into(),
            }),
            TokenTree::Token(Token {
                token_type: Identifier,
                span: Span::empty(),
            }),
        ];

        assert_eq!(input, expected);
    }

    #[test]
    fn test_tokentree_open_with_span() {
        let input = test_tokentree!({:4, Identifier, Identifier});
        let expected = vec![TokenTree::Group(Group {
            open: Token {
                token_type: OpenCurly,
                span: 4.into(),
            },
            children: vec![
                TokenTree::Token(Token {
                    token_type: Identifier,
                    span: Span::empty(),
                }),
                TokenTree::Token(Token {
                    token_type: Identifier,
                    span: Span::empty(),
                }),
            ],
            close: Some(Token {
                token_type: CloseCurly,
                span: Span::empty(),
            }),
        })];

        assert_eq!(input, expected);
    }

    #[test]
    fn test_tokentree_close_with_span() {
        let input = test_tokentree!({Identifier, Identifier}:5);
        let expected = vec![TokenTree::Group(Group {
            open: Token {
                token_type: OpenCurly,
                span: Span::empty(),
            },
            children: vec![
                TokenTree::Token(Token {
                    token_type: Identifier,
                    span: Span::empty(),
                }),
                TokenTree::Token(Token {
                    token_type: Identifier,
                    span: Span::empty(),
                }),
            ],
            close: Some(Token {
                token_type: CloseCurly,
                span: 5.into(),
            }),
        })];

        assert_eq!(input, expected);
    }


    #[test]
    fn test_tokens_with_spans() {
        let input = test_tokens!(Use, Identifier:2..3, Equals:5, Identifier);
        let expected = vec![
            Token {
                token_type: Use,
                span: Span::empty(),
            },
            Token {
                token_type: Identifier,
                span: (2..3).into(),
            },
            Token {
                token_type: Equals,
                span: 5.into(),
            },
            Token {
                token_type: Identifier,
                span: Span::empty(),
            },
        ];

        assert_eq!(input, expected);
    }

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
        let input = test_tokentree!(Identifier, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier, Semicolon).iter().collect::<Vec<_>>());
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn recover_finds_matching_not_immediate() {
        // arrange
        let input = test_tokentree!(Unknown, Identifier, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);
        token_starter!(semicolon, Semicolon);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], [semicolon]);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier, Semicolon).iter().collect::<Vec<_>>());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                span: Span::empty(),
            }]
        );
    }

    #[test]
    fn recover_finds_error_cases_not_immediate() {
        // arrange
        let input = test_tokentree!(Unknown, Identifier, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);
        token_starter!(semicolon, Semicolon);

        // act
        let found = recover_until(&mut iter, &mut errors, [semicolon], [identifier]);

        // assert
        assert_eq!(found, false);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier, Semicolon).iter().collect::<Vec<_>>());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                span: Span::empty(),
            }]
        );
    }

    #[test]
    fn recover_only_one_unexpected_error() {
        // arrange
        let input = test_tokentree!(Unknown, Semicolon, Identifier);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier).iter().collect::<Vec<_>>());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                span: Span::empty(),
            }]
        );
    }

    #[test]
    fn recover_nothing_matches() {
        // arrange
        let input = test_tokentree!(Unknown, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, false);
        assert!(iter.collect::<Vec<_>>().is_empty());
        assert_eq!(
            errors.get_errors(),
            &vec![Error {
                kind: ErrorKind::UnexpectedToken(Unknown),
                span: Span::empty(),
            }]
        );
    }
}
