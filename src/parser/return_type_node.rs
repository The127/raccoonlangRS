use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::{MarkingIterator};
use crate::parser::consume_token;
use crate::parser::type_node::{parse_type, type_starter, TypeNode};
use crate::source_map::Span;
use crate::token_starter;
use crate::tokenizer::TokenType::DashArrow;
use crate::treeizer::TokenTree;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct ReturnTypeNode {
    pub span: Span,
    pub type_node: Option<TypeNode>,
}

pub fn return_type_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
) -> bool {
    token_starter!(dash_arrow, DashArrow);
    dash_arrow(iter)
}

pub fn parse_return_type<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<ReturnTypeNode> {
    let mut mark = iter.mark();
    let mut node = ReturnTypeNode::default();

    if let Some(token) = consume_token(&mut mark, DashArrow) {
        node.span = token.span;
        mark.discard();
    } else {
        mark.reset();
        return None;
    }

    if !type_starter(iter) {
        errors.add(ErrorKind::MissingReturnType, node.span.end);
        return Some(node);
    }

    if let Some(type_node) = parse_type(iter, errors) {
        node.span += type_node.span();
        node.type_node = Some(type_node);
    }

    Some(node)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::NamedType;
    use crate::tokenizer::TokenType::{DashArrow, Identifier, Unknown};
    use crate::treeizer::TokenTree;
    use crate::{test_tokens, test_tokentree};

    #[test]
    fn parse_type_node_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_return_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(
            result,
            Some(ReturnTypeNode {
                span: (2..10).into(),
                type_node: Some(TypeNode::Named(NamedType {
                    span: (5..10).into(),
                    path: PathNode {
                        span: (5..10).into(),
                        is_rooted: false,
                        parts: test_tokens!(Identifier:5..10),
                    }
                },)),
            }),
        );
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_return_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(
            result,
            Some(ReturnTypeNode {
                span: (2..4).into(),
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
