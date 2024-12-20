use crate::marking_iterator::MarkingIterator;
use crate::source_map::Span;

#[derive(Debug, Eq, PartialEq)]
pub struct ModNode {
    span: Span,
}

// 'mod'  ';'
pub fn parse_mods<I: Iterator<Item: Copy>>(iter: impl MarkingIterator<I>) -> Vec<ModNode>
{
    vec![]
}

#[cfg(test)]
mod test {
    use crate::marking_iterator::marking;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_mods_empty(){
        // arrange
        let input: Vec<TokenTree> = vec![];

        // act
        let result = parse_mods(marking(input.iter()));

        // assert
        assert_eq!(result, vec![])
    }
}