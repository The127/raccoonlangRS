use crate::marking_iterator::MarkingIterator;
use crate::parser::mod_node::ModNode;
use crate::parser::use_node::UseNode;
use crate::source_map::Span;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct FileNode {
    span: Span,
    uses: Vec<UseNode>,
    mods: Vec<ModNode>,
}

// TODO: this wants to be tested uwu
pub fn toplevel_starter<'a, I: Iterator<Item = &'a TokenTree>>(iter: &mut dyn MarkingIterator<I>) -> bool {
    let mut mark = iter.mark().auto_reset();

    let result = match mark.next() {
        Some(TokenTree::Token(Token {
            token_type: Mod|Use,
            ..
        })) => true,
        _ => false,
    };

    return result;
}

/// A file starts with uses followed by module declarations.
/// However, to support better compiler errors we parse any top level declaration (a declaration that is within the root token tree) here.
pub fn parse_file(input: Vec<TokenTree>) -> FileNode {
    FileNode {
        span: Span::empty(),
        uses: vec![],
        mods: vec![],
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_file_empty() {
        // arrange
        let input = vec![];

        // act
        let file = parse_file(input);

        // assert
        assert_eq!(
            file,
            FileNode {
                span: Span::empty(),
                uses: vec![],
                mods: vec![],
            }
        )
    }
}
