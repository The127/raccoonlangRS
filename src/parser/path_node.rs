use crate::marking_iterator::MarkingIterator;
use crate::parser::{consume_token, consume_tokens};
use crate::source_map::Span;
use crate::tokenizer::TokenType::{As, Identifier, PathSeparator};
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct PathNode {
    pub span: Span,
    pub parts: Vec<Token>,
    pub is_rooted: bool,
}

pub fn parse_path<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
) -> Option<PathNode> {
    let mut iter = iter.mark();

    let mut node = PathNode {
        span: (0..0).into(),
        parts: vec![],
        is_rooted: false,
    };

    if let Some(root) = consume_token(&mut iter, PathSeparator) {
        node.is_rooted = true;
        node.span = root.span;
    }

    if let Some(first) = consume_token(&mut iter, Identifier) {
        node.span += first.span;
        node.parts.push(first);
    } else {
        iter.reset();
        return None;
    }

    while let Some([sep, id]) = consume_tokens(&mut iter, [PathSeparator, Identifier]) {
        node.span += id.span;
        node.parts.push(id);
    }

    Some(node)
}

pub fn path_starter(tt: &TokenTree) -> bool {
    match tt {
        TokenTree::Token(Token {
            token_type: PathSeparator | Identifier,
            ..
        }) => true,
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::marking_iterator::marking;
    use crate::tokenizer::TokenType::Unknown;
    use crate::{test_token, test_tokens, test_tokentree, test_tokentree_helper};
    
    #[test]
    fn empty_input() {
        // arrange
        let tokens = test_tokentree!();
        let mut iter = marking(tokens.iter());

        // act
        let path = parse_path(&mut iter);

        // assert
        assert_eq!(path, None);
    }

    #[test]
    fn non_matching_input() {
        // arrange
        let tokens = test_tokentree!(Unknown);
        let mut iter = marking(tokens.iter());

        // act
        let path = parse_path(&mut iter);
        let next = iter.next();

        // assert
        assert_eq!(path, None);
    }

    #[test]
    fn single_identifier() {
        // arrange
        let tokens = test_tokentree!(Identifier);
        let mut iter = marking(tokens.iter());

        // act
        let path = parse_path(&mut iter);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier),
                is_rooted: false,
                span: (0..0).into(),
            })
        );
    }

    #[test]
    fn rooted_identifier() {
        // arrange
        let tokens = test_tokentree!(PathSeparator Identifier);
        let mut iter = marking(tokens.iter());

        // act
        let path = parse_path(&mut iter);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier),
                is_rooted: true,
                span: (0..0).into(),
            })
        );
    }

    #[test]
    fn two_identifiers() {
        // arrange
        let tokens = test_tokentree!(Identifier PathSeparator Identifier);
        let mut iter = marking(tokens.iter());

        // act
        let path = parse_path(&mut iter);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier Identifier),
                is_rooted: false,
                span: (0..0).into(),
            })
        );
    }

    #[test]
    fn two_identifiers_rooted() {
        // arrange
        let tokens = test_tokentree!(PathSeparator Identifier PathSeparator Identifier);
        let mut iter = marking(tokens.iter());

        // act
        let path = parse_path(&mut iter);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier Identifier),
                is_rooted: true,
                span: (0..0).into(),
            })
        );
    }
}
