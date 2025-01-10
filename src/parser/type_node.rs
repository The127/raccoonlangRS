use crate::errors::Errors;
use crate::awesome_iterator::AwesomeIterator;
use crate::parser::path_node::{parse_path, path_starter, PathNode};
use crate::source_map::{HasSpan, Span};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub enum TypeNode {
    Named(NamedTypeNode),
}

impl TypeNode {
    pub fn span(&self) -> Span {
        match self {
            TypeNode::Named(named_type) => named_type.span_,
        }
    }
}

pub fn type_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    path_starter(iter)
}

pub fn parse_type<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<TypeNode> {
    let path = parse_path(iter, errors)?;

    Some(TypeNode::Named(NamedTypeNode {
        span_: path.span(),
        path: path,
    }))
}

#[derive(Debug, Eq, PartialEq)]
pub struct NamedTypeNode {
    span_: Span,
    pub path: PathNode,
}

impl HasSpan for NamedTypeNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl NamedTypeNode {
    pub fn new<S: Into<Span>>(span: S, path: PathNode) -> Self {
        Self {
            span_: span.into(),
            path,
        }
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use crate::errors::Errors;
    use crate::awesome_iterator::make_awesome;
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::{parse_type, NamedTypeNode, TypeNode};
    use crate::tokenizer::TokenType::{Identifier, Unknown};
    use crate::treeizer::TokenTree;
    use crate::{test_tokens, test_tokentree};

    #[test]
    fn parse_type_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_type(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_type_wrong_token() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown:2..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Unknown:2..10).iter().collect::<Vec<_>>()
        )
    }

    #[test]
    fn parse_type_path() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_type(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(TypeNode::Named(_)));
        assert!(errors.get_errors().is_empty());
    }
}
