mod file_node;
mod mod_node;
mod use_node;
mod path_node;

use crate::marking_iterator::MarkingIterator;
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::TokenTree;

pub fn expect_token<'a, I: Iterator<Item = &'a TokenTree>>(iter: &mut I, token_type: TokenType) -> Token {
    match iter.next() {
        Some(TokenTree::Token(token)) if token.token_type == token_type => token.clone(),
        Some(TokenTree::Token(token)) => panic!("expected: {:?}, found: {:?}", token_type, token.token_type),
        Some(TokenTree::Group(group)) => panic!("expected: {:?}, found: {:?}", token_type, group.open.token_type),
        _ => panic!("expected: {:?}, found: None", token_type),
    }
}

//TODO: this wants tests
pub fn consume_token<'a, I: Iterator<Item = &'a TokenTree>>(iter: &mut impl MarkingIterator<I>, token_type: TokenType) -> Option<Token> {
    let mut mark = iter.mark();
    if let Some(TokenTree::Token(token)) = mark.next() {
        if token.token_type == token_type {
            return Some(*token);
        }
    }
    mark.reset();
    return None;
}

//TODO: this wants tests
fn consume_tokens<'a, const count: usize, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    types: [TokenType; count],
) -> Option<[Token; count]> {
    let mut mark = iter.mark();

    let mut tokens : [Token; count] = unsafe { std::mem::zeroed() };

    for i in 0..types.len() {
        let token_type = types[i];
        match mark.next() {
            Some(TokenTree::Token(token)) if token.token_type == token_type => {
                tokens[i] = *token;
            }
            _ => {
                mark.reset();
                return None;
            },
        }
    }

    return Some(tokens);
}

macro_rules! recover_until {
    () => {};
}

mod test_utils {

    #[macro_export]
    macro_rules! test_token {
        ($name:ident) => {
            Token {
                token_type: $name,
                span: (0..0).into(),
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
                        span: (0..0).into(),
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
    use crate::tokenizer::Token;
    use crate::tokenizer::TokenType::{Identifier, Mod};

    #[test]
    fn expect_is_found() {
        // arrange
        let input = vec![TokenTree::Token(Token {
            token_type: Mod,
            span: (0..0).into(),
        })];

        // act
        expect_token(&mut input.iter(), Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];

        // act
        expect_token(&mut input.iter(), Mod);
    }

    #[test]
    #[should_panic]
    fn expect_is_not_found() {
        // arrange
        let input = vec![TokenTree::Token(Token {
            token_type: Identifier,
            span: (0..0).into(),
        })];

        // act
        expect_token(&mut input.iter(), Mod);
    }
}
