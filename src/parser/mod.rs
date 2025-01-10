pub mod file_node;
pub mod mod_node;
pub mod path_node;
pub mod use_node;
pub mod fn_node;
pub mod type_node;
pub mod return_type_node;
pub mod fn_parameter_node;
pub mod literal_expression_node;
pub mod expression_node;
pub mod block_expression_node;
pub mod add_expression_node;
pub mod compare_expression_node;
pub mod if_expression_node;
pub mod let_declaration_node;
pub mod access_expression_node;

use crate::awesome_iterator::AwesomeIterator;
use crate::errors::{ErrorKind, Errors};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::{Group, TokenTree};

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub enum Visibility {
    #[default]
    Module,
    Public(Token),
}

#[derive(Default, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Spanned<T> {
    span_: Span,
    pub value: T,
}

impl<T> HasSpan for Spanned<T> {
    fn span(&self) -> Span {
        self.span_
    }
}

impl<T> Spanned<T> {
    pub fn new<S: Into<Span>>(span: S, value: T) -> Self {
        Self {
            span_: span.into(),
            value,
        }
    }
}

pub type RecoverMatcher<I>
= fn(iter: &mut dyn AwesomeIterator<I>) -> bool;

/// Returns true if a matcher matches, false if the iterator ends or an error_case matches.
fn recover_until<'a, const MATCH_COUNT: usize, const ERROR_COUNT: usize, I>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
    matchers: [RecoverMatcher<I>; MATCH_COUNT],
    error_cases: [RecoverMatcher<I>; ERROR_COUNT],
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
                    unexpected.span(),
                );
                errored_already = true;
            }
            Some(TokenTree::Group(unexpected)) if !errored_already => {
                errors.add(
                    ErrorKind::UnexpectedToken(unexpected.open.token_type),
                    unexpected.open.span(),
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
    ($name:ident, $($token_type:ident)|+) => {
        fn $name<'a, I: core::iter::Iterator<Item = &'a crate::treeizer::TokenTree>>(iter: &mut dyn crate::awesome_iterator::AwesomeIterator<I>) -> bool {
            match iter.peek() {
                Some(crate::treeizer::TokenTree::Token(crate::tokenizer::Token {
                    token_type: $(crate::tokenizer::TokenType::$token_type)|+,
                    ..
                })) => true,
                _ => false,
            }
        }
    };
}

#[macro_export]
macro_rules! group_starter {
    ($name:ident, $($token_type:ident)|+) => {
        fn $name<'a, I: core::iter::Iterator<Item = &'a crate::treeizer::TokenTree>>(iter: &mut dyn crate::awesome_iterator::AwesomeIterator<I>) -> bool {
            match iter.peek() {
                Some(crate::treeizer::TokenTree::Group(crate::treeizer::Group {
                    open:
                        crate::tokenizer::Token {
                            token_type: $(crate::tokenizer::TokenType::$token_type)|+,
                            ..
                        },
                    ..
                })) => true,
                _ => false,
            }
        }
    };
}

#[macro_export]
macro_rules! expect_token {
    ($iter:expr, $($token_type:ident)|+) => {
        match $iter.next() {
            Some(crate::treeizer::TokenTree::Token(token @ crate::tokenizer::Token {
                token_type: $(crate::tokenizer::TokenType::$token_type)|+, ..
            })) => *token,
            Some(crate::treeizer::TokenTree::Token(token)) => {
                panic!("expected: {:?}, found: {:?}", stringify!($($token_type)|+), token.token_type)
            }
            Some(crate::treeizer::TokenTree::Group(group)) => panic!(
                "expected: {:?}, found: {:?}",
                stringify!($($token_type)|+), group.open.token_type
            ),
            _ => panic!("expected: {:?}, found: None", stringify!($($token_type)|+)),
        }
    };
}

//TODO: this wants tests
pub fn consume_group<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
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

#[macro_export]
macro_rules! consume_token {
    ($iter:expr, $($token_type:ident)|+) => {
        {
            let mut mark = $iter.mark();
            match mark.next() {
                Some(crate::treeizer::TokenTree::Token(token @ crate::tokenizer::Token {
                    token_type: $(crate::tokenizer::TokenType::$token_type)|+, ..
                })) => Some(*token),
                _ => {
                    mark.reset();
                    None
                },
            }
        }
    };
}

//TODO: this wants tests
fn consume_tokens<'a, const COUNT: usize, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    types: [TokenType; COUNT],
) -> Option<[Token; COUNT]> {
    let mut mark = iter.mark();

    let mut tokens: [Token; COUNT] = unsafe { std::mem::zeroed() };

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

#[cfg(test)]
pub mod test_utils {
    use crate::awesome_iterator::{make_awesome, AwesomeIterator};
    use crate::errors::Errors;
    use crate::source_map::SourceCollection;
    use crate::tokenizer::tokenize;
    use crate::treeizer::{treeize, TokenTree};
    use std::slice::Iter;

    pub fn test_parse_from_string<T>(sources: &mut SourceCollection, input: &str,
                                 parser: impl Fn(&mut dyn AwesomeIterator<Iter<TokenTree>>,
                                     &mut Errors,
                                 ) -> Option<T>) -> T {
        let mut errors = Errors::new();
        let span = sources.load_content(input);
        let tokenizer = tokenize(span, &sources);
        let tt = treeize(tokenizer);
        let mut iter = make_awesome(tt.iter());
        let result = parser(&mut iter, &mut errors).unwrap();
        result
    }


    #[macro_export]
    macro_rules! test_token {
        ($name:ident) => {
            crate::tokenizer::Token::new($name, 0..0)
        };
        ($name:ident : $span:expr) => {
            crate::tokenizer::Token::new($name, $span)
        };
    }

    #[macro_export]
    macro_rules! test_tokens {
        (@token $name:ident) => {
            crate::tokenizer::Token::new($name, 0..0)
        };
        (@token $name:ident, $span:expr) => {
            crate::tokenizer::Token::new($name, $span)
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
            crate::tokenizer::Token::new($name, 0..0)
        };
        (@token $name:ident, $span:expr) => {
            crate::tokenizer::Token::new($name, $span)
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
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::source_map::Span;
    use crate::tokenizer::Token;
    use crate::tokenizer::TokenType::*;
    use crate::{test_tokens, test_tokentree};

    #[test]
    fn test_tokentree_with_spans() {
        let input = test_tokentree!(Use, Identifier:2..3, Equals:5, Identifier);
        let expected = vec![
            TokenTree::Token(Token::new(Use, Span::empty())),
            TokenTree::Token(Token::new(Identifier, 2..3)),
            TokenTree::Token(Token::new(Equals, 5)),
            TokenTree::Token(Token::new(Identifier, Span::empty())),
        ];

        assert_eq!(input, expected);
    }

    #[test]
    fn test_tokentree_open_with_span() {
        let input = test_tokentree!({:4, Identifier, Identifier});
        let expected = vec![TokenTree::Group(Group {
            open: Token::new(OpenCurly, 4),
            children: vec![
                TokenTree::Token(Token::new(Identifier, Span::empty())),
                TokenTree::Token(Token::new(Identifier, Span::empty())),
            ],
            close: Some(Token::new(CloseCurly, Span::empty())),
        })];

        assert_eq!(input, expected);
    }

    #[test]
    fn test_tokentree_close_with_span() {
        let input = test_tokentree!({Identifier, Identifier}:5);
        let expected = vec![TokenTree::Group(Group {
            open: Token::new(OpenCurly, Span::empty()),
            children: vec![
                TokenTree::Token(Token::new(Identifier, Span::empty())),
                TokenTree::Token(Token::new(Identifier, Span::empty())),
            ],
            close: Some(Token::new(CloseCurly, 5)),
        })];

        assert_eq!(input, expected);
    }


    #[test]
    fn test_tokens_with_spans() {
        let input = test_tokens!(Use, Identifier:2..3, Equals:5, Identifier);
        let expected = vec![
            Token::new(Use, Span::empty()),
            Token::new(Identifier, 2..3),
            Token::new(Equals, 5),
            Token::new(Identifier, Span::empty()),
        ];

        assert_eq!(input, expected);
    }

    #[test]
    fn expect_is_found() {
        // arrange
        let input = test_tokentree!(Mod);
        let mut iter = input.iter();

        // act
        expect_token!(&mut iter, Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];
        let mut iter = input.iter();

        // act
        expect_token!(&mut iter, Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_not_found() {
        // arrange
        let input = test_tokentree!(Identifier);
        let mut iter = input.iter();

        // act
        expect_token!(&mut iter, Mod);
    }

    #[test]
    fn recover_finds_matching_immediate() {
        // arrange
        let input = test_tokentree!(Identifier, Semicolon);
        let mut iter = make_awesome(input.iter());
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
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);
        token_starter!(semicolon, Semicolon);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], [semicolon]);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier, Semicolon).iter().collect::<Vec<_>>());
        assert!(errors.has_error_at(Span::empty(), UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn recover_finds_error_cases_not_immediate() {
        // arrange
        let input = test_tokentree!(Unknown, Identifier, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);
        token_starter!(semicolon, Semicolon);

        // act
        let found = recover_until(&mut iter, &mut errors, [semicolon], [identifier]);

        // assert
        assert_eq!(found, false);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier, Semicolon).iter().collect::<Vec<_>>());
        assert!(errors.has_error_at(Span::empty(), UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn recover_only_one_unexpected_error() {
        // arrange
        let input = test_tokentree!(Unknown, Semicolon, Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, true);
        assert_eq!(iter.collect::<Vec<_>>(), test_tokentree!(Identifier).iter().collect::<Vec<_>>());
        assert!(errors.has_error_at(Span::empty(), UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn recover_nothing_matches() {
        // arrange
        let input = test_tokentree!(Unknown, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        token_starter!(identifier, Identifier);

        // act
        let found = recover_until(&mut iter, &mut errors, [identifier], []);

        // assert
        assert_eq!(found, false);
        assert!(iter.collect::<Vec<_>>().is_empty());
        assert!(errors.has_error_at(Span::empty(), UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn tokenstarter() {
        // arrange
        let input = test_tokentree!(Unknown, DecInteger, Equals, Semicolon);
        let mut iter = make_awesome(input.iter());
        token_starter!(equals_starter, Equals);

        // act
        let matches1 = equals_starter(&mut iter);
        iter.next();
        let matches2 = equals_starter(&mut iter);
        iter.next();
        let matches3 = equals_starter(&mut iter);
        iter.next();
        let matches4 = equals_starter(&mut iter);

        // assert
        assert_eq!(matches1, false);
        assert_eq!(matches2, false);
        assert_eq!(matches3, true);
        assert_eq!(matches4, false);
    }
}
