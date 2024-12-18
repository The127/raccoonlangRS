mod file_node;
mod mod_node;
mod use_node;

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
