use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::MarkingIterator;
use crate::parser::{consume_group, consume_token, recover_until};
use crate::parser::file_node::toplevel_starter;
use crate::source_map::Span;
use crate::{token_starter, group_starter};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{Fn, Identifier, OpenCurly, OpenParen, Pub};
use crate::treeizer::TokenTree;

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub enum Visibility {
    #[default]
    Module,
    Public(Token),
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct FnNode {
    pub span: Span,
    pub visibility: Visibility,
    pub name: Option<Token>,
}

pub fn parse_fn<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<FnNode>
{
    let mut result = FnNode::default();

    let mut mark = iter.mark();

    token_starter!(fn_starter, Fn);

    if let Some(pub_token) = consume_token(&mut mark, Pub) {
        result.span = pub_token.span;
        result.visibility = Visibility::Public(pub_token);

        let mut recover_errors = Errors::new();
        if !recover_until(&mut mark, &mut recover_errors, [fn_starter], [toplevel_starter]) {
            mark.reset();
            return None;
        }

        errors.merge(recover_errors);
    }

    if let Some(fn_token) = consume_token(&mut mark, Fn) {
        result.span += fn_token.span;
        mark.discard();
    } else {
        mark.reset();
        return None;
    };


    token_starter!(identifier, Identifier);
    group_starter!(param_starter, OpenParen);
    group_starter!(body_starter, OpenCurly);

    if !recover_until(iter, errors, [identifier, param_starter, body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingFunctionName, result.span.end);
        errors.add(ErrorKind::MissingFunctionParameterList, result.span.end);
        errors.add(ErrorKind::MissingFunctionBody, result.span.end);
        return Some(result);
    }

    if let Some(name) = consume_token(iter, Identifier) {
        result.span += name.span;
        result.name = Some(name);
    } else {
        errors.add(ErrorKind::MissingFunctionName, result.span.end);
    }

    if !recover_until(iter, errors, [param_starter, body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingFunctionParameterList, result.span.end);
        errors.add(ErrorKind::MissingFunctionBody, result.span.end);
        return Some(result);
    }

    if let Some(params) = consume_group(iter, OpenParen) {
        result.span += params.span();
    } else {
        errors.add(ErrorKind::MissingFunctionParameterList, result.span.end);
    }

    if !recover_until(iter, errors, [body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingFunctionBody, result.span.end);
        return Some(result);
    }

    if let Some(body) = consume_group(iter, OpenCurly) {
        result.span += body.span();
    } else {
        unreachable!("no body after recovering to it");
    }

    Some(result)
}


#[cfg(test)]
mod test {
    use crate::marking_iterator::marking;
    use crate::{test_token, test_tokens, test_tokentree};
    use crate::errors::ErrorKind;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_fn_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_fn_trivial() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:12,):13, {:15, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
        }));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_pub_fn() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:1..3, Fn:4..5, Identifier:6..10, (:12,):13, {:15, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (1..21).into(),
            visibility: Visibility::Public(test_token!(Pub:1..3)),
            name: Some(test_token!(Identifier:6..10)),
        }));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_fn_missing_name() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, (:12,):13, {:15, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: None,
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn parse_fn_missing_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, {:15, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
        }));
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionParameterList));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn parse_fn_missing_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:12,):13,);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..14).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
        }));
        assert!(errors.has_error_at(14, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn parse_fn_missing_name_and_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, {:15, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: None,
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionParameterList));
        assert_eq!(errors.get_errors().len(), 2);
    }

    #[test]
    fn parse_fn_missing_name_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, (:12,):13);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..14).into(),
            visibility: Visibility::Module,
            name: None,
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert!(errors.has_error_at(14, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 2);
    }

    #[test]
    fn parse_fn_missing_params_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..10).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
        }));
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionParameterList));
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 2);
    }

    #[test]
    fn parse_fn_missing_everything() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..5).into(),
            visibility: Visibility::Module,
            name: None,
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionParameterList));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 3);
    }

    #[test]
    fn parse_fn_unexpected_tokens() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Unknown:6..8, Identifier:9..12, Unknown:13..15, (:16,):17, Unknown:20..25, {:25, }:28);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..29).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:9..12)),
        }));
        assert!(errors.has_error_at(6..8, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(13..15, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(20..25, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 3);
    }
}