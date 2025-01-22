use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::fn_node::FnNode;
use crate::source_map::{HasSpan, Span};
use crate::{add_error, consume_token, expect_token, token_starter};
use crate::parser::{consume_group, recover_until, Visibility};
use crate::parser::file_node::toplevel_starter;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::OpenParen;
use crate::treeizer::TokenTree;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct StructNode {
    span_: Span,
    pub visibility: Visibility,
    pub name: Option<Token>,
}

impl HasSpan for StructNode {
    fn span(&self) -> Span {
        self.span_
    }
}


pub fn parse_struct<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<StructNode> {
    let mut result = StructNode::default();

    let mut mark = iter.mark();

    token_starter!(struct_starter, Struct);

    if let Some(pub_token) = consume_token!(&mut mark, Pub) {
        result.span_ = pub_token.span();
        result.visibility = Visibility::Public(pub_token);

        let mut recover_errors = Errors::new();
        if !recover_until(
            &mut mark,
            &mut recover_errors,
            [struct_starter],
            [toplevel_starter],
        ) {
            mark.reset();
            return None;
        }

        errors.merge(recover_errors);
    }

    if let Some(struct_token) = consume_token!(&mut mark, Struct) {
        result.span_ += struct_token.span();
        mark.discard();
    } else {
        mark.reset();
        return None;
    };

    if let Some(name) = consume_token!(iter, Identifier) {
        result.span_ += name.span();
        result.name = Some(name);
    } else {
        add_error!(errors, result.span_.end(), MissingDeclarationName);
    }

    let group = consume_group(iter, OpenParen).unwrap();
    result.span_ += group.span();


    Some(result)
}


#[cfg(test)]
mod test {
    use ustr::ustr;
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::{ErrorKind, Errors};
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::{Identifier, Pub, Struct};
    use crate::treeizer::TokenTree;

    #[test]
    fn empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn empty_struct() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Struct:1..7, Identifier:8..11, (:12, ):13 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(StructNode {
            span_: Span(1, 14),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:8..11)),
        }));
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn pub_struct() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:3..6, Struct:10..16, Identifier:18..21, (:22, ):23 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(StructNode {
            span_: Span(3, 24),
            visibility: Visibility::Public(test_token!(Pub:3..6)),
            name: Some(test_token!(Identifier:18..21)),
        }));
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn missing_name() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:3..6, Struct:10..16, (:22, ):23 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(StructNode {
            span_: Span(3, 24),
            visibility: Visibility::Public(test_token!(Pub:3..6)),
            name: None,
        }));
        assert!(remaining.is_empty());
        assert!(errors.has_error_at(16, ErrorKind::MissingDeclarationName));
        assert_eq!(errors.get_errors().len(), 1);
    }
}