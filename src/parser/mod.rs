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
pub mod mul_expression_node;
pub mod tuple_expression_node;
pub mod pattern_node;
pub mod struct_node;
pub mod subsequent_expression_node;
pub mod new_expression_node;
pub mod arg_node;

use std::ops::{Deref, DerefMut};
use crate::add_error;
use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
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


pub trait ToSpanned<T> {
    fn spanned<S: Into<Span>>(self, span: S) -> Spanned<T>;

    #[cfg(test)]
    fn spanned_empty(self) -> Spanned<T>;
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> ToSpanned<T> for T {
    fn spanned<S: Into<Span>>(self, span: S) -> Spanned<T> {
        Spanned::new(span, self)
    }

    #[cfg(test)]
    fn spanned_empty(self) -> Spanned<T> {
        Spanned::new(Span::empty(), self)
    }
}

impl<T> HasSpan for Spanned<T> {
    #[mutants::skip]
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
            Some(TokenTree::Token(token)) if token.token_type == TokenType::DocComment => (),
            Some(TokenTree::Token(unexpected)) if !errored_already => {
                add_error!(errors, unexpected.span(), UnexpectedToken(unexpected.token_type));
                errored_already = true;
            }
            Some(TokenTree::Group(unexpected)) if !errored_already => {
                add_error!(errors, unexpected.open.span(), UnexpectedToken(unexpected.open.token_type));
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
        {
            loop {
                match $iter.peek() {
                    Some(crate::treeizer::TokenTree::Token(token)) if token.token_type == crate::tokenizer::TokenType::DocComment => {
                        $iter.next();
                        continue
                    },
                    _ => break,
                };
            }
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
        }
    };
}


//TODO: this wants tests
pub fn consume_group<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    token_type: TokenType,
) -> Option<&'a Group> {
    let mut mark = iter.mark();
    loop {
        match mark.peek() {
            Some(TokenTree::Token(token)) if token.token_type == TokenType::DocComment => {
                mark.next();
                continue
            },
            _ => break,
        };
    }
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
            loop {
                match mark.peek() {
                    Some(crate::treeizer::TokenTree::Token(token)) if token.token_type == crate::tokenizer::TokenType::DocComment => {
                        mark.next();
                        continue
                    },
                    _ => break,
                };
            }
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



#[macro_export]
macro_rules! seq_expression {
    ($name:ident, $next:expr, $($op:ident)|+, $node_kind:ident, $node_type:ident, $follow_type:ident) => {
        pub fn $name <'a, I: Iterator<Item = &'a TokenTree>>(
            iter: &mut dyn AwesomeIterator<I>,
            errors: &mut Errors,
            greedy_after_block: bool,
        ) -> Option<ExpressionNode> {
            if let Some(left) = $next(iter, errors, greedy_after_block) {
                if left.is_block() && !greedy_after_block {
                    return Some(left);
                }

                let mut result = $node_type {
                    span_: left.span(),
                    left: Box::new(left),
                    follows: vec![],
                };

                loop {
                    let operator_token = match consume_token!(iter, $($op)|+) {
                        Some(operator_token) => operator_token,
                        None => break,
                    };

                    result.span_ += operator_token.span();

                    let right = if let Some(follow) = $next(iter, errors, true) {
                        result.span_ += follow.span();
                        Some(follow)
                    } else {
                        add_error!(errors, result.span_.end(), MissingOperand);
                        None
                    };

                    result.follows.push($follow_type {
                        operator: operator_token,
                        operand: right,
                    });
                }

                if result.follows.is_empty() {
                    return Some(*result.left);
                }

                return Some(crate::parser::expression_node::ExpressionNode::$node_kind(result));
            }

            None
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
            crate::tokenizer::Token::new(0..0, $name)
        };
        ($name:ident : $span:expr) => {
            crate::tokenizer::Token::new($span, $name)
        };
    }

    #[macro_export]
    macro_rules! test_tokens {
        (@token $name:ident) => {
            crate::tokenizer::Token::new(0..0, $name)
        };
        (@token $name:ident, $span:expr) => {
            crate::tokenizer::Token::new($span, $name)
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
        (@token $name:expr) => {
            crate::tokenizer::Token::new(0..0, $name)
        };
        (@token $name:expr, $span:expr) => {
            crate::tokenizer::Token::new($span, $name)
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
        (@single $token_type:ident) => {
            crate::treeizer::TokenTree::Token(test_tokentree!(@token $token_type))
        };
        (@single $token_type:ident, $span:expr) => {
            crate::treeizer::TokenTree::Token(test_tokentree!(@token $token_type, $span))
        };
        (@group $opener:ident, $closer:ident, [:$open_span:expr, $($children:tt)*] $(, $close_span:expr)?) => {
            crate::treeizer::TokenTree::Group(crate::treeizer::Group {
                open: test_tokentree!(@token crate::tokenizer::TokenType::$opener, $open_span),
                close: Some(test_tokentree!(@token crate::tokenizer::TokenType::$closer $(, $close_span)?)),
                children: test_tokentree!($($children)*),
            })
        };
        (@group $opener:ident, $closer:ident, [$($children:tt)*] $(, $close_span:expr)?) => {
            crate::treeizer::TokenTree::Group(crate::treeizer::Group {
                open: test_tokentree!(@token crate::tokenizer::TokenType::$opener),
                close: Some(test_tokentree!(@token crate::tokenizer::TokenType::$closer $(, $close_span)?)),
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
            TokenTree::Token(Token::new(Span::empty(), Use)),
            TokenTree::Token(Token::new(2..3, Identifier)),
            TokenTree::Token(Token::new(5, Equals)),
            TokenTree::Token(Token::new(Span::empty(), Identifier)),
        ];

        assert_eq!(input, expected);
    }

    #[test]
    fn test_tokentree_open_with_span() {
        let input = test_tokentree!({:4, Identifier, Identifier});
        let expected = vec![TokenTree::Group(Group {
            open: Token::new(4, OpenCurly),
            children: vec![
                TokenTree::Token(Token::new(Span::empty(), Identifier)),
                TokenTree::Token(Token::new(Span::empty(), Identifier)),
            ],
            close: Some(Token::new(Span::empty(), CloseCurly)),
        })];

        assert_eq!(input, expected);
    }

    #[test]
    fn test_tokentree_close_with_span() {
        let input = test_tokentree!({Identifier, Identifier}:5);
        let expected = vec![TokenTree::Group(Group {
            open: Token::new(Span::empty(), OpenCurly),
            children: vec![
                TokenTree::Token(Token::new(Span::empty(), Identifier)),
                TokenTree::Token(Token::new(Span::empty(), Identifier)),
            ],
            close: Some(Token::new(5, CloseCurly)),
        })];

        assert_eq!(input, expected);
    }


    #[test]
    fn test_tokens_with_spans() {
        let input = test_tokens!(Use, Identifier:2..3, Equals:5, Identifier);
        let expected = vec![
            Token::new(Span::empty(), Use),
            Token::new(2..3, Identifier),
            Token::new(5, Equals),
            Token::new(Span::empty(), Identifier),
        ];

        assert_eq!(input, expected);
    }

    #[test]
    fn expect_is_found() {
        // arrange
        let input = test_tokentree!(Mod);
        let mut iter = make_awesome(input.iter());

        // act
        expect_token!(&mut iter, Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];
        let mut iter = make_awesome(input.iter());

        // act
        expect_token!(&mut iter, Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_not_found() {
        // arrange
        let input = test_tokentree!(Identifier);
        let mut iter = make_awesome(input.iter());

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
        errors.assert_empty();
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
