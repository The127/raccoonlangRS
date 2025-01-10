use crate::errors::{ErrorKind, Errors};
use crate::awesome_iterator::{AwesomeIterator};
use crate::parser::type_node::{parse_type, type_starter, TypeNode};
use crate::source_map::{HasSpan, Span};
use crate::{consume_token, token_starter};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::DashArrow;
use crate::treeizer::TokenTree;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct ReturnTypeNode {
    span_: Span,
    pub type_node: Option<TypeNode>,
}

impl HasSpan for ReturnTypeNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl ReturnTypeNode {
    pub fn new<S: Into<Span>>(span: S, type_node: Option<TypeNode>) -> Self {
        Self {
            span_: span.into(),
            type_node,
        }
    }
}

pub fn return_type_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    token_starter!(dash_arrow, DashArrow);
    dash_arrow(iter)
}

pub fn parse_return_type<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ReturnTypeNode> {
    let mut mark = iter.mark();
    let mut node = ReturnTypeNode::default();

    if let Some(token) = consume_token!(&mut mark, DashArrow) {
        node.span_ = token.span();
        mark.discard();
    } else {
        mark.reset();
        return None;
    }

    if !type_starter(iter) {
        errors.add(ErrorKind::MissingReturnType, node.span_.end());
        return Some(node);
    }

    if let Some(type_node) = parse_type(iter, errors) {
        node.span_ += type_node.span();
        node.type_node = Some(type_node);
    }

    Some(node)
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use super::*;
    use crate::errors::Errors;
    use crate::awesome_iterator::make_awesome;
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::NamedTypeNode;
    use crate::tokenizer::TokenType::{DashArrow, Identifier, Unknown};
    use crate::treeizer::TokenTree;
    use crate::{test_tokens, test_tokentree};

    #[test]
    fn parse_type_node_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_return_type(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_type_node_unknown() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_return_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        )
    }

    #[test]
    fn parse_type_node_named_type() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DashArrow:2..4, Identifier:5..10, Unknown:11..12);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_return_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_matches!(result, Some(ReturnTypeNode {
            type_node: Some(TypeNode::Named(_)),
            ..
        }));
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Unknown:11..12).iter().collect::<Vec<_>>()
        )
    }

    #[test]
    fn parse_type_node_type_missing() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DashArrow:2..4, Unknown:5..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_return_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(
            result,
            Some(ReturnTypeNode {
                span_: (2..4).into(),
                type_node: None,
            }),
        );
        assert!(errors.has_error_at(4, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(
            remaining,
            test_tokentree!(Unknown:5..10).iter().collect::<Vec<_>>()
        )
    }
}
