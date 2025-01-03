use ustr::{ustr, Ustr};
use crate::ast::Visibility;
use crate::parser::fn_node::FnNode;
use crate::parser::fn_parameters::FnParameterNode;
use crate::source_map::{SourceCollection, Span};

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDecl {
    pub span: Span,
    pub name: Ustr,
    pub visibility: Visibility,
    pub parameters: Vec<FunctionParameter>
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionParameter {
    pub span: Span,
    pub name: Ustr,
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
        parameters: node.parameters
            .iter()
            .map(|p| transform_function_param(p, sources).unwrap())
            // .filter_map(|x| x)
            .collect(),
    })
}

fn transform_function_param(node: &FnParameterNode, sources: &SourceCollection) -> Option<FunctionParameter> {
    Some(FunctionParameter {
        span: node.span,
        name: sources.get_identifier(node.name.span),
    })
}


#[cfg(test)]
mod test{
    use super::*;
    use parameterized::parameterized;
    use ustr::ustr;
    use crate::ast::function_decl::{transform_function_decl, FunctionDecl};
    use crate::parser::fn_node::FnNode;
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::{Identifier, Pub};
    use crate::parser::{Visibility as ParserVisibility};
    use crate::parser::fn_parameters::FnParameterNode;

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
        assert_eq!(decl, Some(FunctionDecl {
            span: span,
            name: ustr(name),
            visibility: Visibility::Module,
            parameters: vec![],
        }));
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
        assert_eq!(decl, Some(FunctionDecl {
            span: Span::empty(),
            name: ustr(""),
            visibility: Visibility::Public,
            parameters: vec![],
        }));
    }

    #[parameterized(param_names = {
        vec!["foo", "bar", "foobar"],
        vec!["hello", "world"],
    })]
    fn transform_function_decl_params(param_names: Vec<&str>) {
        // arrange
        let mut sources = SourceCollection::new();
        let name_span = sources.load_content("".to_string());

        let names_with_spans = param_names.iter().map(|name| (name, sources.load_content(name.to_string()))).collect::<Vec<_>>();


        let param_name_span = sources.load_content("foo".to_string());
        let fn_node = FnNode {
            span: Span::empty(),
            visibility: ParserVisibility::Module,
            name: Some(test_token!(Identifier:name_span)),
            parameters: names_with_spans.iter().map(|(_, span)| FnParameterNode {
                span: *span,
                name: test_token!(Identifier:*span),
                type_: None,
            }).collect(),
            return_type: None,
            body: None,
        };

        // act
        let decl = transform_function_decl(&fn_node, &sources);

        // assert
        assert_eq!(decl, Some(FunctionDecl {
            span: Span::empty(),
            name: ustr(""),
            visibility: Visibility::Module,
            parameters: names_with_spans.iter().map(|(name, span)| FunctionParameter {
                span: *span,
                name: ustr(name),
            }).collect(),
        }));
    }
}