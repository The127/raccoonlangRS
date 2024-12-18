use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct File {

}

// parse_uses parse_mods
pub fn parse_file(input: Vec<TokenTree>) -> File {
    File{}
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
        assert_eq!(file, File{})
    }
}