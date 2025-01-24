use crate::ast::function_decl::FunctionParameter;
use crate::ast::types::Type::Unknown;
use crate::ast::types::{transform_type, Type};
use crate::ast::{map_visibility, Visibility};
use crate::errors::Errors;
use crate::parser::fn_parameter_node::FnParameterNode;
use crate::parser::struct_node::{StructMemberNode, StructNode};
use crate::source_map::{HasSpan, SourceCollection, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDecl {
    span_: Span,
    pub name: Option<Ustr>,
    pub visibility: Visibility,
    pub members: Vec<StructMember>,
}

impl HasSpan for StructDecl {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructMember {
    span_: Span,
    name: Ustr,
    type_: Type,
}

impl HasSpan for StructMember {
    fn span(&self) -> Span {
        self.span_
    }
}

impl StructMember {
    pub fn new<S: Into<Span>>(span: S, name: Ustr, type_: Type) -> Self {
        Self {
            span_: span.into(),
            name,
            type_,
        }
    }
}

pub fn transform_struct_decl(
    node: &StructNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> StructDecl {
    StructDecl {
        span_: node.span(),
        name: node.name.map(|x| sources.get_identifier(x.span())),
        visibility: map_visibility(node.visibility),
        members: node
            .members
            .iter()
            .map(|m| transform_struct_member(m, sources))
            .collect(),
    }
}

fn transform_struct_member(node: &StructMemberNode, sources: &SourceCollection) -> StructMember {
    let type_ = match &node.type_ {
        Some(t) => transform_type(&t, sources),
        None => Unknown,
    };

    StructMember::new(node.span(), sources.get_identifier(node.name.span()), type_)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::path::Path;
    use crate::ast::types::NamedType;
    use crate::parser::type_node::{NamedTypeNode, TypeNode};
    use crate::parser::{Spanned, Visibility as ParserVisibility};
    use crate::{test_token, test_tokens};
    use crate::tokenizer::TokenType::{Identifier, Pub};
    use assert_matches::assert_matches;
    use parameterized::{ide, parameterized};
    use ustr::ustr;
    use crate::parser::path_node::PathNode;

    #[parameterized(
        values = {
            ("Foobar", (10..20).into()),
            ("Asdf", (7..15).into()),
        }
    )]
    fn transform_function_decl_name(values: (&str, Span)) {
        let (name, span) = values;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let name_span = sources.load_content(name);
        let struct_node = StructNode::new(
            span,
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![],
        );

        // act
        let decl = transform_struct_decl(&struct_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            StructDecl {
                span_: span,
                name: Some(ustr(name)),
                visibility: Visibility::Module,
                members: vec![],
            }
        )
    }

    #[test]
    fn transform_function_decl_public() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let name_span = sources.load_content("");
        let struct_node = StructNode::new(
            Span::empty(),
            ParserVisibility::Public(test_token!(Pub:0..3)),
            Some(test_token!(Identifier:name_span)),
            vec![],
        );

        // act
        let decl = transform_struct_decl(&struct_node, &mut errors, &sources);

        // assert
        assert_matches!(
            decl,
            StructDecl {
                visibility: Visibility::Public,
                ..
            }
        )
    }

    #[test]
    fn transform_function_decl_one_member() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let name_span = sources.load_content("");
        let member_span = sources.load_content("foo");
        let bar_span = sources.load_content("Bar");
        let struct_node = StructNode::new(
            Span::empty(),
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![StructMemberNode::new(
                Span::empty(),
                test_token!(Identifier:member_span),
                Some(TypeNode::Named(NamedTypeNode::new(
                    bar_span,
                    PathNode::new(
                        bar_span,
                        test_tokens!(Identifier:bar_span),
                        false,
                    ),
                ))),
            )],
        );

        // act
        let decl = transform_struct_decl(&struct_node, &mut errors, &sources);

        // assert
        assert_matches!(
            decl,
            StructDecl{
                members,
                ..
            } => assert_matches!(&members[..], [
                StructMember{
                    name,
                    type_,
                    ..
                }
            ] => {
                assert_eq!(*name, ustr("foo"));
                assert_eq!(*type_, Type::Named(NamedType::new(bar_span, Path::name("Bar"))))
            })
        )
    }

    #[test]
    fn transform_function_decl_multiple_members() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let name_span = sources.load_content("");
        let struct_node = StructNode::new(
            Span::empty(),
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![
                StructMemberNode::new(Span::empty(), test_token!(Identifier), None),
                StructMemberNode::new(Span::empty(), test_token!(Identifier), None),
            ],
        );

        // act
        let decl = transform_struct_decl(&struct_node, &mut errors, &sources);

        // assert
        assert_matches!(
            decl,
            StructDecl{
                members,
                ..
            } => assert_matches!(members[..], [
                StructMember{..},
                StructMember{..},
            ])
        )
    }
}
