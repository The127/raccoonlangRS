use crate::ast::types::{transform_type, Type};
use crate::ast::Visibility;
use crate::parser::fn_node::FnNode;
use crate::parser::fn_parameters::FnParameterNode;
use crate::source_map::{SourceCollection, Span};
use ustr::{ustr, Ustr};
use crate::parser::type_node::TypeNode;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDecl {
    pub span: Span,
    pub name: Ustr,
    pub visibility: Visibility,
    pub parameters: Vec<FunctionParameter>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionParameter {
    pub span: Span,
    pub name: Ustr,
    pub type_: Type,
}

pub fn transform_function_decl(node: &FnNode, sources: &SourceCollection) -> Option<FunctionDecl> {
    let name = sources.get_identifier(node.name?.span);

    let visibility = match node.visibility {
        crate::parser::Visibility::Module => Visibility::Module,
        crate::parser::Visibility::Public(_) => Visibility::Public,
    };

    Some(FunctionDecl {
        span: node.span,
        name: name,
        visibility: visibility,
        parameters: node
            .parameters
            .iter()
            .filter_map(|p| transform_function_param(p, sources))
            .collect(),
    })
}

fn transform_function_param(
    node: &FnParameterNode,
    sources: &SourceCollection,
) -> Option<FunctionParameter> {
    let type_ = match &node.type_ {
        Some(t) => transform_type(&t, sources),
        None => Type::Unknown,
    };

    Some(FunctionParameter {
        span: node.span,
        name: sources.get_identifier(node.name.span),
        type_: type_,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::function_decl::{transform_function_decl, FunctionDecl};
    use crate::ast::types::NamedType;
    use crate::parser::fn_node::FnNode;
    use crate::parser::fn_parameters::FnParameterNode;
    use crate::parser::{Spanned, Visibility as ParserVisibility};
    use crate::source_map::{SourceCollection, Span};
    use crate::{test_token, test_tokens};
    use crate::tokenizer::TokenType::{Identifier, Pub};
    use parameterized::parameterized;
    use ustr::ustr;
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::TypeNode;

    #[parameterized(
        values = {
            ("foobar", (10..20).into()),
            ("asdf", (7..15).into()),
        }
    )]
    fn transform_function_decl_name(values: (&str, Span)) {
        let (name, span) = values;
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content(name.to_string());
        let fn_node = FnNode {
            span: span,
            visibility: ParserVisibility::Module,
            name: Some(test_token!(Identifier:name_span)),
            parameters: vec![],
            return_type: None,
            body: None,
        };

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            Some(FunctionDecl {
                span: span,
                name: ustr(name),
                visibility: Visibility::Module,
                parameters: vec![],
            })
        );
    }

    #[test]
    fn transform_function_decl_public() {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("".to_string());
        let fn_node = FnNode {
            span: Span::empty(),
            visibility: ParserVisibility::Public(test_token!(Pub:0..3)),
            name: Some(test_token!(Identifier:name_span)),
            parameters: vec![],
            return_type: None,
            body: None,
        };

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            Some(FunctionDecl {
                span: Span::empty(),
                name: ustr(""),
                visibility: Visibility::Public,
                parameters: vec![],
            })
        );
    }

    #[parameterized(param_names = {
        vec![("foo", "Foo"), ("bar", "Bar"), ("foobar", "FooBar")],
        vec![("hello", "Hello"), ("world", "World")],
    })]
    fn transform_function_decl_params(param_names: Vec<(&str, &str)>) {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("".to_string());

        let strs_with_spans: Vec<_> = param_names
            .iter()
            .map(|(name, typename)| {
                (
                    Spanned {
                        value: name,
                        span: sources.load_content(name.to_string()),
                    },
                    Spanned {
                        value: typename,
                        span: sources.load_content(typename.to_string()),
                    },
                )
            })
            .collect();

        let param_name_span = sources.load_content("foo".to_string());
        let fn_node = FnNode {
            span: Span::empty(),
            visibility: ParserVisibility::Module,
            name: Some(test_token!(Identifier:name_span)),
            parameters: strs_with_spans
                .iter()
                .map(|(name, typename)| FnParameterNode {
                    span: name.span,
                    name: test_token!(Identifier:name.span),
                    type_: Some(TypeNode::Named(crate::parser::type_node::NamedTypeNode {
                        span: typename.span,
                        path: PathNode {
                            span: typename.span,
                            parts: test_tokens!(Identifier:typename.span),
                            is_rooted: false,
                        },
                    })),
                })
                .collect(),
            return_type: None,
            body: None,
        };

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            Some(FunctionDecl {
                span: Span::empty(),
                name: ustr(""),
                visibility: Visibility::Module,
                parameters: strs_with_spans
                    .iter()
                    .map(|(name, typename)| FunctionParameter {
                        span: name.span,
                        name: ustr(name.value),
                        type_: Type::Named(NamedType {
                            span: typename.span,
                            path: vec![ustr(typename.value)],
                            rooted: false,
                        }),
                    })
                    .collect(),
            })
        );
    }
}
