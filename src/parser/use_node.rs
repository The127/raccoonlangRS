use std::ops::Add;
use crate::marking_iterator::MarkingIterator;
use crate::parser::*;
use crate::parser::path_node::{parse_path, PathNode};
use crate::source_map::Span;
use crate::tokenizer::{Token};
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct UseNode {
    pub span: Span,
    pub path: Option<PathNode>
}

pub fn parse_use<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
) -> Option<UseNode> {
    let use_token = consume_token(iter, Use)?;

    let mut iter = iter.mark();

    // if !recover_until!(iter, [path_starter], [toplevel_starter]) {
    //     // TODO register error
    //     iter.discard();
    //     return Some(UseNode {span: use_token.span, path: None});
    // }

    let path = parse_path(&mut iter).expect("no path");

    return Some(UseNode {
        span: use_token.span + path.span,
        path: Some(path),
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::marking_iterator::marking;
    use crate::{test_token, test_tokens};
    use crate::tokenizer::Token;
    use crate::treeizer::TokenTree;

    #[test]
    fn parse_uses_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];

        // act
        let result = parse_use(&mut marking(input.iter()));

        // assert
        assert_eq!(result, None)
    }

    #[test]
    fn parse_uses_one_use() {
        // arrange
        let input: Vec<TokenTree> = vec![
            TokenTree::Token(Token {
                token_type: Use,
                span: (0..0).into(),
            }),
            TokenTree::Token(Token {
                token_type: Identifier,
                span: (0..0).into(),
            }),
            TokenTree::Token(Token {
                token_type: PathSeparator,
                span: (0..0).into(),
            }),
            TokenTree::Token(Token {
                token_type: Identifier,
                span: (0..0).into(),
            }),
            TokenTree::Token(Token {
                token_type: Semicolon,
                span: (0..0).into(),
            }),
        ];

        // act
        let result = parse_use(&mut marking(input.iter()));

        // assert
        assert_eq!(result, Some(UseNode {
            span: Span { start: 0, end: 0 },
            path: Some(PathNode {
                span: Span { start: 0, end: 0 },
                is_rooted: false,
                parts: test_tokens!(Identifier Identifier),
            }),
        }));
    }
}
