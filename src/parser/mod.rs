mod use_node;
mod mod_node;

use crate::parser::mod_node::ModNode;
use crate::parser::use_node::UseNode;
use crate::source_map::Span;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct FileNode {
    span: Span,
    uses: Vec<UseNode>,
    mods: Vec<ModNode>,
}

// parse_uses parse_mods
pub fn parse_file(input: Vec<TokenTree>) -> FileNode {
    FileNode {
        span: (0..0).into(),
        uses: vec![],
        mods: vec![],
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_file_empty(){
        // arrange
        let input = vec![];

        // act
        let file = parse_file(input);

        // assert
        assert_eq!(file, FileNode {
            span: (0..0).into(),
            uses: vec![],
            mods: vec![],
        })
    }
}