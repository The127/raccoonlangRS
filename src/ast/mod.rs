pub mod mod_part;
pub mod function_decl;
pub mod types;
pub mod parse_transform;
pub mod statement;
pub mod typing;
pub mod path;
pub mod pattern;
pub mod struct_decl;
pub mod expressions;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Visibility {
    Module,
    Public,
}

pub fn map_visibility(parser_visibility: crate::parser::Visibility) -> Visibility{
    match parser_visibility {
        crate::parser::Visibility::Module => Visibility::Module,
        crate::parser::Visibility::Public(_) => Visibility::Public,
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{map_visibility, Visibility};
    use crate::test_token;
    use crate::tokenizer::TokenType::Identifier;

    #[test]
    fn module(){
        // arrange
        let parser_visibility = crate::parser::Visibility::Module;

        // act
        let mapped_visibilty = map_visibility(parser_visibility);

        // assert
        assert_eq!(mapped_visibilty, Visibility::Module);
    }

    #[test]
    fn public(){
        // arrange
        let parser_visibility = crate::parser::Visibility::Public(test_token!(Identifier:10..20));

        // act
        let mapped_visibilty = map_visibility(parser_visibility);

        // assert
        assert_eq!(mapped_visibilty, Visibility::Public);
    }
}