use crate::mark_iterator::MarkIterator;
use crate::source_map::Span;

#[derive(Debug, Eq, PartialEq)]
pub struct ModNode {
    span: Span,
}

// 'mod'  ';'
pub fn parse_mods<I: Iterator<Item: Copy>>(iter: MarkIterator<I>) -> Vec<ModNode>
{
    vec![]
}

#[cfg(test)]
mod test {
    use crate::mark_iterator::mark;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_mods_empty(){
        // arrange
        let input: Vec<TokenTree> = vec![];

        // act
        let result = parse_mods(mark(input.iter()));

        // assert
        assert_eq!(result, vec![])
    }
}