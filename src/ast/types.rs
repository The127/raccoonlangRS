use ustr::Ustr;
use crate::source_map::{SourceCollection, Span};
use crate::parser::type_node::{TypeNode, NamedTypeNode};
use crate::source_map::HasSpan;

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Unknown,
    Unit,
    Named(NamedType),
}

pub fn transform_type(node: &TypeNode, sources: &SourceCollection) -> Type {
    match node {
        TypeNode::Named(named_type) => {
            match transform_named_type(named_type, sources) {
                Some(t) => Type::Named(t),
                None => Type::Unknown,
            }
        }
    }
}


#[derive(Debug, Eq, PartialEq)]
pub struct NamedType {
    span_: Span,
    pub path: Vec<Ustr>,
    pub rooted: bool,
}

pub fn transform_named_type(node: &NamedTypeNode, sources: &SourceCollection) -> Option<NamedType> {
    Some(NamedType {
        span_: node.path.span(),
        path: node.path.parts.iter().map(|t| sources.get_identifier(t.span())).collect(),
        rooted: node.path.is_rooted,
    })
}

#[cfg(test)]
mod test {
    use ustr::ustr;
    use crate::parser::path_node::PathNode;
    use super::*;
    use crate::test_tokens;
    use crate::tokenizer::TokenType::Identifier;

    #[test]
    fn transform_named_type_non_rooted_single_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let id_span = sources.load_content("foo");
        let parser_type = TypeNode::Named(NamedTypeNode{
            span_: id_span,
            path: PathNode {
                span_: id_span,
                parts: test_tokens!(Identifier:id_span),
                is_rooted: false,
            },
        });

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(result, Type::Named(NamedType{
            span_: id_span,
            path: vec![ustr("foo")],
            rooted: false,
        }));
    }

    #[test]
    fn transform_named_type_non_rooted_multiple_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let id_span1 = sources.load_content("foo");
        let id_span2 = sources.load_content("bar");
        let parser_type = TypeNode::Named(NamedTypeNode{
            span_: id_span1 + id_span2,
            path: PathNode {
                span_: id_span1 + id_span2,
                parts: test_tokens!(Identifier:id_span1, Identifier:id_span2),
                is_rooted: false,
            },
        });

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(result, Type::Named(NamedType{
            span_: id_span1 + id_span2,
            path: vec![ustr("foo"), ustr("bar")],
            rooted: false,
        }));
    }

    #[test]
    fn transform_named_type_rooted_single_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let root_span = sources.load_content("::");
        let id_span = sources.load_content("foo");
        let parser_type = TypeNode::Named(NamedTypeNode{
            span_: root_span + id_span,
            path: PathNode {
                span_: root_span + id_span,
                parts: test_tokens!(Identifier:id_span),
                is_rooted: true,
            },
        });

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(result, Type::Named(NamedType{
            span_: root_span + id_span,
            path: vec![ustr("foo")],
            rooted: true,
        }));
    }

    #[test]
    fn transform_named_type_rooted_multiple_identifier() {
        // arrange
        let mut sources = SourceCollection::new();
        let root_span = sources.load_content("::");
        let id_span1 = sources.load_content("foo");
        let id_span2 = sources.load_content("bar");
        let parser_type = TypeNode::Named(NamedTypeNode{
            span_: root_span + id_span1 + id_span2,
            path: PathNode {
                span_: root_span + id_span1 + id_span2,
                parts: test_tokens!(Identifier:id_span1, Identifier:id_span2),
                is_rooted: true,
            },
        });

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(result, Type::Named(NamedType{
            span_: root_span + id_span1 + id_span2,
            path: vec![ustr("foo"), ustr("bar")],
            rooted: true,
        }));
    }
}