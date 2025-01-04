use ustr::Ustr;
use crate::source_map::{SourceCollection, Span};
use crate::parser::type_node::{TypeNode, NamedTypeNode};

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Unknown,
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
    pub span: Span,
    pub path: Vec<Ustr>,
    pub rooted: bool,
}

pub fn transform_named_type(node: &NamedTypeNode, sources: &SourceCollection) -> Option<NamedType> {
    Some(NamedType {
        span: node.path.span,
        path: node.path.parts.iter().map(|t| sources.get_identifier(t.span)).collect(),
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
        let id_span = sources.load_content("foo".to_string());
        let parser_type = TypeNode::Named(NamedTypeNode{
            span: id_span,
            path: PathNode {
                span: id_span,
                parts: test_tokens!(Identifier:id_span),
                is_rooted: false,
            },
        });

        // act
        let result = transform_type(&parser_type, &sources);

        // assert
        assert_eq!(result, Type::Named(NamedType{
            span: id_span,
            path: vec![ustr("foo")],
            rooted: false,
        }));
    }
}