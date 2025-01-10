use crate::ast::expressions::{transform_expression, Expression};
use crate::ast::types::Type::{Unit, Unknown};
use crate::ast::types::{transform_type, Type};
use crate::ast::Visibility;
use crate::parser::fn_node::FnNode;
use crate::parser::fn_parameter_node::FnParameterNode;
use crate::parser::return_type_node::ReturnTypeNode;
use crate::source_map::{HasSpan, SourceCollection, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionDecl {
    span_: Span,
    pub name: Option<Ustr>,
    pub visibility: Visibility,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: FunctionReturnType,
    pub body: Expression,
}

impl HasSpan for FunctionDecl {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionParameter {
    span_: Span,
    pub name: Ustr,
    pub type_: Type,
}

impl HasSpan for FunctionParameter {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionReturnType {
    pub type_: Type,
}

pub fn transform_function_decl(node: &FnNode, sources: &SourceCollection) -> FunctionDecl {
    let visibility = match node.visibility {
        crate::parser::Visibility::Module => Visibility::Module,
        crate::parser::Visibility::Public(_) => Visibility::Public,
    };

    FunctionDecl {
        span_: node.span(),
        name: node.name.map(|x| sources.get_identifier(x.span())),
        visibility: visibility,
        parameters: node
            .parameters
            .iter()
            .filter_map(|p| transform_function_param(p, sources))
            .collect(),
        return_type: transform_function_return_type(&node.return_type, sources),
        body: node
            .body
            .as_ref()
            .map(|x| transform_expression(x, sources))
            .unwrap_or(Expression::unknown()),
    }
}

fn transform_function_return_type(
    node: &Option<ReturnTypeNode>,
    sources: &SourceCollection,
) -> FunctionReturnType {
    FunctionReturnType {
        type_: match node {
            Some(return_type) => match &return_type.type_node {
                Some(type_) => transform_type(type_, sources),
                None => Unknown,
            },
            None => Unit,
        },
    }
}

fn transform_function_param(
    node: &FnParameterNode,
    sources: &SourceCollection,
) -> Option<FunctionParameter> {
    let type_ = match &node.type_ {
        Some(t) => transform_type(&t, sources),
        None => Unknown,
    };

    Some(FunctionParameter {
        span_: node.span(),
        name: sources.get_identifier(node.name.span()),
        type_: type_,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::function_decl::{transform_function_decl, FunctionDecl};
    use crate::ast::types::NamedType;
    use crate::ast::types::Type::Unit;
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::fn_node::FnNode;
    use crate::parser::fn_parameter_node::FnParameterNode;
    use crate::parser::path_node::PathNode;
    use crate::parser::return_type_node::ReturnTypeNode;
    use crate::parser::type_node::{NamedTypeNode, TypeNode};
    use crate::parser::{Spanned, Visibility as ParserVisibility};
    use crate::source_map::{SourceCollection, Span};
    use crate::tokenizer::TokenType::{Identifier, Pub};
    use crate::{test_token, test_tokens};
    use parameterized::parameterized;
    use ustr::ustr;

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
        let name_span = sources.load_content(name);
        let fn_node = FnNode::new(
            span,
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![],
            None,
            None,
        );

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl {
                span_: span,
                name: Some(ustr(name)),
                visibility: Visibility::Module,
                parameters: vec![],
                return_type: FunctionReturnType { type_: Unit },
                body: Expression::unknown(),
            }
        );
    }

    #[test]
    fn transform_function_decl_public() {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("");
        let fn_node = FnNode::new(
            Span::empty(),
            ParserVisibility::Public(test_token!(Pub:0..3)),
            Some(test_token!(Identifier:name_span)),
            vec![],
            None,
            None,
        );

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl {
                span_: Span::empty(),
                name: Some(ustr("")),
                visibility: Visibility::Public,
                parameters: vec![],
                return_type: FunctionReturnType { type_: Unit },
                body: Expression::unknown(),
            }
        );
    }

    #[parameterized(param_names = {
        vec![("foo", "Foo"), ("bar", "Bar"), ("foobar", "FooBar")],
        vec![("hello", "Hello"), ("world", "World")],
    })]
    fn transform_function_decl_params(param_names: Vec<(&str, &str)>) {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("");

        let strs_with_spans: Vec<_> = param_names
            .iter()
            .map(|(name, typename)| {
                (
                    Spanned::new(sources.load_content(*name), name),
                    Spanned::new(sources.load_content(*typename), typename),
                )
            })
            .collect();

        let fn_node = FnNode::new(
            Span::empty(),
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            strs_with_spans
                .iter()
                .map(|(name, typename)| {
                    FnParameterNode::new(
                        name.span(),
                        test_token!(Identifier:name.span()),
                        Some(TypeNode::Named(NamedTypeNode::new(
                            typename.span(),
                            PathNode::new(
                                typename.span(),
                                test_tokens!(Identifier:typename.span()),
                                false,
                            ),
                        ))),
                    )
                })
                .collect(),
            None,
            None,
        );

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl {
                span_: Span::empty(),
                name: Some(ustr("")),
                visibility: Visibility::Module,
                parameters: strs_with_spans
                    .iter()
                    .map(|(name, typename)| FunctionParameter {
                        span_: name.span(),
                        name: ustr(name.value),
                        type_: Type::Named(NamedType::new(
                            typename.span(),
                            vec![ustr(typename.value)],
                            false,
                        )),
                    })
                    .collect(),
                return_type: FunctionReturnType { type_: Unit },
                body: Expression::unknown(),
            }
        );
    }

    #[test]
    fn transform_function_decl_return_type() {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("");
        let return_type_span = sources.load_content("Foo");

        let fn_node = FnNode::new(
            Span::empty(),
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![],
            Some(ReturnTypeNode::new(
                5..10,
                Some(TypeNode::Named(NamedTypeNode::new(
                    return_type_span,
                    PathNode::new(
                        return_type_span,
                        test_tokens!(Identifier:return_type_span),
                        false,
                    ),
                ))),
            )),
            None,
        );

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl {
                span_: Span::empty(),
                name: Some(ustr("")),
                visibility: Visibility::Module,
                parameters: vec![],
                return_type: FunctionReturnType {
                    type_: Type::Named(NamedType::new(
                        return_type_span,
                        vec![ustr("Foo")],
                        false,
                    )),
                },
                body: Expression::unknown(),
            }
        );
    }

    #[test]
    fn transform_function_decl_return_type_missing() {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("");

        let fn_node = FnNode::new(
            Span::empty(),
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![],
            Some(ReturnTypeNode::new(5..10, None)),
            None,
        );

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl {
                span_: Span::empty(),
                name: Some(ustr("")),
                visibility: Visibility::Module,
                parameters: vec![],
                return_type: FunctionReturnType { type_: Unknown },
                body: Expression::unknown(),
            }
        );
    }

    #[test]
    fn transform_function_body() {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("foo");
        let body_span = sources.load_content("{}");

        let fn_node = FnNode::new(
            name_span + body_span,
            ParserVisibility::Module,
            Some(test_token!(Identifier:name_span)),
            vec![],
            None,
            Some(ExpressionNode::Block(BlockExpressionNode::new(
                body_span,
                vec![],
                None,
            ))),
        );

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl {
                span_: name_span + body_span,
                name: Some(ustr("foo")),
                visibility: Visibility::Module,
                parameters: vec![],
                return_type: FunctionReturnType { type_: Unit },
                body: Expression::block(body_span, vec![], None),
            }
        );
    }
}
