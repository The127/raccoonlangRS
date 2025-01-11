use crate::parser::type_node::{NamedTypeNode, TypeNode};
use crate::source_map::HasSpan;
use crate::source_map::{SourceCollection, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Unknown,
    Unit,
    Named(NamedType),
}


pub fn transform_type(node: &TypeNode, sources: &SourceCollection) -> Type {
    match node {
        TypeNode::Named(named_type) => match transform_named_type(named_type, sources) {
            Some(t) => Type::Named(t),
            None => Type::Unknown,
        },
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedType {
    span_: Span,
    pub path: Vec<Ustr>,
    pub rooted: bool,
}

impl HasSpan for NamedType {
    fn span(&self) -> Span {
        self.span_
    }
}

impl NamedType {
    pub fn new<S: Into<Span>>(span: S, path: Vec<Ustr>, rooted: bool) -> Self {
        Self {
            span_: span.into(),
            path,
            rooted,
        }
    }
}

pub fn transform_named_type(node: &NamedTypeNode, sources: &SourceCollection) -> Option<NamedType> {
    Some(NamedType {
        span_: node.path.span(),
        path: node
            .path
            .parts
            .iter()
            .map(|t| sources.get_identifier(t.span()))
            .collect(),
        rooted: node.path.is_rooted,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::path_node::PathNode;
    use crate::test_tokens;
    use crate::tokenizer::TokenType::Identifier;
    use ustr::ustr;

    #[test]
    fn transform_named_type_non_rooted_single_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let id_span = sources.load_content("foo");
        let parser_type = TypeNode::Named(NamedTypeNode::new(
            id_span,
            PathNode::new(id_span, test_tokens!(Identifier:id_span), false),
        ));

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(
            result,
            Type::Named(NamedType {
                span_: id_span,
                path: vec![ustr("foo")],
                rooted: false,
            })
        );
    }

    #[test]
    fn transform_named_type_non_rooted_multiple_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let id_span1 = sources.load_content("foo");
        let id_span2 = sources.load_content("bar");
        let parser_type = TypeNode::Named(NamedTypeNode::new(
            id_span1 + id_span2,
            PathNode::new(
                id_span1 + id_span2,
                test_tokens!(Identifier:id_span1, Identifier:id_span2),
                false,
            ),
        ));

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(
            result,
            Type::Named(NamedType {
                span_: id_span1 + id_span2,
                path: vec![ustr("foo"), ustr("bar")],
                rooted: false,
            })
        );
    }

    #[test]
    fn transform_named_type_rooted_single_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let root_span = sources.load_content("::");
        let id_span = sources.load_content("foo");
        let parser_type = TypeNode::Named(NamedTypeNode::new(
            root_span + id_span,
            PathNode::new(
                root_span + id_span,
                test_tokens!(Identifier:id_span),
                true,
            ),
        ));

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(
            result,
            Type::Named(NamedType {
                span_: root_span + id_span,
                path: vec![ustr("foo")],
                rooted: true,
            })
        );
    }

    #[test]
    fn transform_named_type_rooted_multiple_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let root_span = sources.load_content("::");
        let id_span1 = sources.load_content("foo");
        let id_span2 = sources.load_content("bar");
        let parser_type = TypeNode::Named(NamedTypeNode::new(
            root_span + id_span1 + id_span2,
            PathNode::new(
                root_span + id_span1 + id_span2,
                test_tokens!(Identifier:id_span1, Identifier:id_span2),
                true,
            ),
        ));

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(
            result,
            Type::Named(NamedType {
                span_: root_span + id_span1 + id_span2,
                path: vec![ustr("foo"), ustr("bar")],
                rooted: true,
            })
        );
    }
}
