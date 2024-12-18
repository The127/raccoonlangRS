use crate::mark_iterator::MarkIterator;
use crate::source_map::Span;

#[derive(Debug, Eq, PartialEq)]
pub struct UseNode {
    span: Span,
}

// 'use'  ';'
pub fn parse_uses<I: Iterator<Item: Copy>>(iter: MarkIterator<I>) -> Vec<UseNode>
{
    vec![]
}

#[cfg(test)]
mod test {
    use crate::mark_iterator::mark;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_uses_empty(){
        // arrange
        let input: Vec<TokenTree> = vec![];

        // act
        let result = parse_uses(mark(input.iter()));

        // assert
        assert_eq!(result, vec![])
    }
}