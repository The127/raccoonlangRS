use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::fn_node::{parse_fn, FnNode};
use crate::parser::mod_node::{parse_mod, ModNode};
use crate::parser::recover_until;
use crate::parser::struct_node::{parse_struct, StructNode};
use crate::parser::use_node::{parse_use, UseNode};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct FileNode {
    pub decls: Vec<TopLevelDeclaration>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevelDeclaration {
    Use(UseNode),
    Mod(ModNode),
    Fn(FnNode),
    Struct(StructNode),
}

// TODO: this wants to be tested uwu
pub fn toplevel_starter<'a, I: Iterator<Item=&'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    let mut mark = iter.mark().auto_reset();

    let result = match mark.next() {
        Some(TokenTree::Token(Token {
                                  token_type: Mod | Use | Fn | Struct | Pub,
                                  ..
                              })) => true,
        _ => false,
    };

    result
}

/// A file starts with uses followed by module declarations.
/// However, to support better compiler errors we parse any top level declaration (a declaration that is within the root token tree) here.
pub fn parse_file<'a, I: Iterator<Item=&'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> FileNode {
    let mut result = FileNode {
        decls: vec![],
    };

    while recover_until(iter, errors, [toplevel_starter], []) {
        if let Some(use_node) = parse_use(iter, errors) {
            result.decls.push(TopLevelDeclaration::Use(use_node));
        } else if let Some(mod_node) = parse_mod(iter, errors) {
            result.decls.push(TopLevelDeclaration::Mod(mod_node));
        } else if let Some(fn_node) = parse_fn(iter, errors) {
            result.decls.push(TopLevelDeclaration::Fn(fn_node));
        } else if let Some(struct_node) = parse_struct(iter, errors) {
            result.decls.push(TopLevelDeclaration::Struct(struct_node));
        } else {
            break;
        };
    }

    result
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind;
    use crate::source_map::HasSpan;
    use crate::test_tokentree;
    use assert_matches::assert_matches;
    use crate::parser::struct_node::StructNode;

    #[test]
    fn parse_file_empty() {
        // arrange
        let input = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![]
            }
        );
        errors.assert_empty();
    }

    #[test]
    fn parse_file_single_use() {
        let input = test_tokentree!(Use, Identifier, PathSeparator, Identifier, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Use(UseNode {
                ..
            })
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_multiple_uses() {
        let input = test_tokentree!(
            Use, Identifier, PathSeparator, Identifier, Semicolon,
            Use, Identifier, PathSeparator, {Identifier, Comma, Identifier}, Semicolon,
        );


        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Use(UseNode {multi: None, ..}),
            TopLevelDeclaration::Use(UseNode {multi: Some(_), ..}),
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_single_mod() {
        let input = test_tokentree!(Mod, Identifier, PathSeparator, Identifier, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Mod(ModNode {..})
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_multiple_mods() {
        let input = test_tokentree!(
            Mod,
            Identifier,
            PathSeparator,
            Identifier,
            Semicolon,
            Mod,
            Identifier,
            Semicolon,
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Mod(ModNode {..}),
            TopLevelDeclaration::Mod(ModNode {..}),
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_single_fn() {
        // arrange
        let input = test_tokentree!(Fn, Identifier, (), DashArrow, Identifier, {});
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Fn(FnNode {..}),
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_single_struct() {
        // arrange
        let input = test_tokentree!(Struct, Identifier, ());
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Struct(StructNode {..}),
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_interspersed() {
        let input = test_tokentree!(
            Mod, Identifier, PathSeparator, Identifier, Semicolon,
            Struct, Identifier, (),
            Use, Identifier, Semicolon,
            Pub, Fn, Identifier, (), DashArrow, Identifier, {},
            Mod, Identifier, Semicolon,
            Use, PathSeparator, Identifier, Semicolon,
            Pub, Struct, Identifier, (),
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(file.decls[..], [
            TopLevelDeclaration::Mod(_),
            TopLevelDeclaration::Struct(_),
            TopLevelDeclaration::Use(_),
            TopLevelDeclaration::Fn(_),
            TopLevelDeclaration::Mod(_),
            TopLevelDeclaration::Use(_),
            TopLevelDeclaration::Struct(_),
        ]);
        errors.assert_empty();
    }

    #[test]
    fn parse_file_unexpected_tokens() {
        let input = test_tokentree!(
            Unknown:1..3,
            Mod:5..8, Identifier:9..14, Semicolon:14,
            Unknown:15..20,
            Use:23..26, Identifier:27..30, Semicolon:30,
            Unknown:35..40,
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(&file.decls[..], [
            TopLevelDeclaration::Mod(m),
            TopLevelDeclaration::Use(u),
        ] if m.span() == (5..14).into() && u.span() == (23..30).into());

        assert!(errors.has_error_at(1..3, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(15..20, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(35..40, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 3);
    }

    #[test]
    fn parse_file_missing_semicolon() {
        let input = test_tokentree!(
            Mod:5..8, Identifier:9..14,
            Use:23..26, Identifier:27..30, Semicolon:30,
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_matches!(&file.decls[..], [
            TopLevelDeclaration::Mod(m),
            TopLevelDeclaration::Use(u),
        ] if m.span() == (5..14).into() && u.span() == (23..30).into());

        assert!(errors.has_error_at(14, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
    }
}
