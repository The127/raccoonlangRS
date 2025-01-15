use crate::awesome_iterator::AwesomeIterator;
use crate::errors::{ErrorKind, Errors};
use crate::parser::file_node::toplevel_starter;
use crate::parser::path_node::{parse_path, path_starter, PathNode};
use crate::parser::recover_until;
use crate::source_map::{HasSpan, Span};
use crate::treeizer::TokenTree;
use crate::{consume_token, expect_token, token_starter};

#[derive(Debug, Eq, PartialEq)]
pub struct ModNode {
    span_: Span,
    pub path: Option<PathNode>,
}

impl HasSpan for ModNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_mod<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ModNode>
{
    let mut result = match consume_token!(iter, Mod) {
        None => {
            return None;
        }
        Some(mod_token) => ModNode {
            span_: mod_token.span(),
            path: None,
        }
    };

    token_starter!(semicolon, Semicolon);
    if !recover_until(iter, errors,[path_starter, semicolon], [toplevel_starter]) {
        errors.add(ErrorKind::MissingModulePath, result.span_.end());
        errors.add(ErrorKind::MissingSemicolon, result.span_.end());
        return Some(result);
    }

    if let Some(path) = parse_path(iter, errors) {
        result.span_ += path.span();
        result.path = Some(path);
    } else {
        errors.add(ErrorKind::MissingModulePath, result.span_.end());
    }

    if !recover_until(iter, errors,[semicolon], [toplevel_starter]) {
        errors.add(ErrorKind::MissingSemicolon, result.span_.end());
        return Some(result);
    }

    expect_token!(iter, Semicolon);

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::treeizer::TokenTree;
    use crate::test_tokentree;
    use assert_matches::assert_matches;
    use crate::tokenizer::TokenType::*;

    #[test]
    fn parse_mod_empty(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mod(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_mod_no_errors(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Mod:10..13, Identifier:15..20, Semicolon:21);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mod(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(m @ ModNode {
            path: Some(_),
            ..
        }) if m.span() == (10..20).into());

        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_mod_unexpected_after_path(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Mod:10..13, Identifier:15..20, PathSeparator:20..22, Semicolon:22);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mod(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(m @ ModNode {
            path: Some(_),
            ..
        }) if m.span() == (10..20).into());
        assert!(errors.has_error_at(20..22, UnexpectedToken(PathSeparator)));
    }

    #[test]
    fn parse_mod_missing_path(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Mod:10..13, Semicolon:14);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mod(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(ModNode {
            span_: (10..13).into(),
            path: None,
        }));

        assert!(errors.has_error_at(13, ErrorKind::MissingModulePath));
    }

    #[test]
    fn parse_mod_missing_semicolon(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Mod:10..13, Identifier:14..20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mod(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(m @ ModNode {
            path: Some(_),
            ..
        }) if m.span() == (10..20).into());
        assert!(errors.has_error_at(20, ErrorKind::MissingSemicolon));
    }

    #[test]
    fn parse_mod_missing_path_and_missing_semicolon(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Mod:10..13);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mod(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(ModNode {
            span_: (10..13).into(),
            path: None,
        }));

        assert!(errors.has_error_at(13, ErrorKind::MissingModulePath));
        assert!(errors.has_error_at(13, ErrorKind::MissingSemicolon));
    }
}