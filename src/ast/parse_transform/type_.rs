use crate::ast::path::Path;
use crate::ast::types::{NamedType, Type};
use crate::parser::type_node::{NamedTypeNode, TypeNode};
use crate::source_map::{HasSpan, SourceCollection};

pub fn transform_type(node: &TypeNode, sources: &SourceCollection) -> Type {
    match node {
        TypeNode::Named(named_type) => match transform_named_type(named_type, sources) {
            Some(t) => Type::Named(t),
            None => Type::Unknown,
        },
    }
}

pub fn transform_named_type(node: &NamedTypeNode, sources: &SourceCollection) -> Option<NamedType> {
    Some(NamedType::new(
        node.path.span(),
        Path::new(
            node.path
                .parts
                .iter()
                .map(|t| sources.get_identifier(t.span()))
                .collect(),
            node.path.is_rooted,
        ),
    ))
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
            Type::Named(NamedType::new(id_span, Path::name("foo")))
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
            Type::Named(NamedType::new(
                id_span1 + id_span2,
                Path::new(vec![ustr("foo"), ustr("bar")], false)
            ))
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
            PathNode::new(root_span + id_span, test_tokens!(Identifier:id_span), true),
        ));

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(
            result,
            Type::Named(NamedType::new(
                root_span + id_span,
                Path::new(vec![ustr("foo")], true)
            ))
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
            Type::Named(NamedType::new(
                root_span + id_span1 + id_span2,
                Path::new(vec![ustr("foo"), ustr("bar")], true)
            ))
        );
    }
}
