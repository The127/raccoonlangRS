use crate::ast::expressions::Expression;
use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
use crate::ast::map_visibility;
use crate::ast::parse_transform::transform_expression;
use crate::ast::parse_transform::type_::transform_type;
use crate::ast::types::Type::{Unit, Unknown};
use crate::errors::Errors;
use crate::parser::fn_node::FnNode;
use crate::parser::fn_parameter_node::FnParameterNode;
use crate::parser::return_type_node::ReturnTypeNode;
use crate::source_map::{HasSpan, SourceCollection};

pub fn transform_function_decl(
    node: &FnNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> FunctionDecl {
    FunctionDecl::new(
        node.span(),
        node.name.map(|x| sources.get_identifier(x.span())),
        map_visibility(node.visibility),
        node.parameters
            .iter()
            .map(|p| transform_function_param(p, sources))
            .collect(),
        transform_function_return_type(&node.return_type, sources),
        node.body
            .as_ref()
            .map(|x| transform_expression(x, errors, sources))
            .unwrap_or(Expression::unknown()),
    )
}

fn transform_function_return_type(
    node: &Option<ReturnTypeNode>,
    sources: &SourceCollection,
) -> FunctionReturnType {
    FunctionReturnType::new(match node {
        Some(return_type) => match &return_type.type_node {
            Some(type_) => transform_type(type_, sources),
            None => Unknown,
        },
        None => Unit,
    })
}

fn transform_function_param(
    node: &FnParameterNode,
    sources: &SourceCollection,
) -> FunctionParameter {
    let type_ = match &node.type_ {
        Some(t) => transform_type(&t, sources),
        None => Unknown,
    };

    FunctionParameter::new(node.span(), sources.get_identifier(node.name.span()), type_)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::path::Path;
    use crate::ast::types::Type::Unit;
    use crate::ast::types::{NamedType, Type};
    use crate::ast::Visibility;
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
        let mut errors = Errors::new();
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
        let decl = transform_function_decl(&fn_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl::new(
                span,
                Some(ustr(name)),
                Visibility::Module,
                vec![],
                FunctionReturnType::new(Unit),
                Expression::unknown(),
            )
        );
    }

    #[test]
    fn transform_function_decl_public() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
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
        let decl = transform_function_decl(&fn_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl::new(
                Span::empty(),
                Some(ustr("")),
                Visibility::Public,
                vec![],
                FunctionReturnType::new(Unit),
                Expression::unknown()
            )
        );
    }

    #[parameterized(param_names = {
        vec![("foo", "Foo"), ("bar", "Bar"), ("foobar", "FooBar")],
        vec![("hello", "Hello"), ("world", "World")],
    })]
    fn transform_function_decl_params(param_names: Vec<(&str, &str)>) {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
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
        let decl = transform_function_decl(&fn_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl::new(
                Span::empty(),
                Some(ustr("")),
                Visibility::Module,
                strs_with_spans
                    .iter()
                    .map(|(name, typename)| FunctionParameter::new(
                        name.span(),
                        ustr(name),
                        Type::Named(NamedType::new(typename.span(), Path::name(*typename.value)))
                    ))
                    .collect(),
                FunctionReturnType::new(Unit),
                Expression::unknown()
            )
        );
    }

    #[test]
    fn transform_function_decl_return_type() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
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
        let decl = transform_function_decl(&fn_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl::new(
                Span::empty(),
                Some(ustr("")),
                Visibility::Module,
                vec![],
                FunctionReturnType::new(Type::Named(NamedType::new(
                    return_type_span,
                    Path::name("Foo")
                ))),
                Expression::unknown()
            )
        );
    }

    #[test]
    fn transform_function_decl_return_type_missing() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
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
        let decl = transform_function_decl(&fn_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl::new(
                Span::empty(),
                Some(ustr("")),
                Visibility::Module,
                vec![],
                FunctionReturnType::new(Unknown),
                Expression::unknown()
            )
        );
    }

    #[test]
    fn transform_function_body() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
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
        let decl = transform_function_decl(&fn_node, &mut errors, &sources);

        // assert
        assert_eq!(
            decl,
            FunctionDecl::new(
                name_span + body_span,
                Some(ustr("foo")),
                Visibility::Module,
                vec![],
                FunctionReturnType::new(Unit),
                Expression::block(body_span, vec![], None)
            )
        );
    }
}
