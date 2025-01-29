use crate::ast::expressions::arg::Arg;
use crate::ast::expressions::Expression;
use crate::ast::parse_transform::path::transform_path;
use crate::ast::parse_transform::transform_expression;
use crate::errors::Errors;
use crate::parser::arg_node::ArgNode;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::new_expression_node::NewExpressionNode;
use crate::parser::ToSpanned;
use crate::source_map::{HasSpan, SourceCollection};

pub fn transform_new_expression(
    node: &NewExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    let args = node
        .args
        .value
        .iter()
        .map(|arg| match arg {
            ArgNode::Named(named) => Arg::named(
                named.span(),
                sources
                    .get_identifier(named.name.span())
                    .spanned(named.name.span()),
                transform_expression(named.value.as_ref().unwrap(), errors, sources),
            ),
            ArgNode::Unnamed(unnamed) => match unnamed.value.as_ref() {
                ExpressionNode::Access(access) => Arg::shorthand(
                    unnamed.span(),
                    sources
                        .get_identifier(access.identifier.span())
                        .spanned(access.identifier.span()),
                    transform_expression(&unnamed.value, errors, sources),
                ),
                _ => todo!(),
            },
        })
        .collect();

    Expression::new_(
        node.span(),
        transform_path(node.path.as_ref().unwrap(), sources),
        args,
    )
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::arg::Arg;
    use crate::ast::expressions::Expression;
    use crate::ast::parse_transform::new::transform_new_expression;
    use crate::ast::path::Path;
    use crate::errors::Errors;
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::arg_node::{ArgNode, NamedArgNode, UnnamedArgNode};
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::parser::new_expression_node::NewExpressionNode;
    use crate::parser::path_node::PathNode;
    use crate::parser::ToSpanned;
    use crate::source_map::SourceCollection;
    use crate::tokenizer::TokenType::{DecInteger, Identifier};
    use crate::{test_token, test_tokens};
    use ustr::ustr;

    #[test]
    fn no_args() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("new Foo()");
        let span_name = span.sub(4..7);
        let span_args = span.sub(7..);

        let node = NewExpressionNode::new(
            span,
            Some(PathNode::new(
                span_name,
                test_tokens!(Identifier:span_name),
                false,
            )),
            vec![].spanned(span_args),
        );

        // act
        let expr = transform_new_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::new_(span, Path::name("Foo"), vec![]));
        errors.assert_empty();
    }

    #[test]
    fn normal_arg() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("new Foo(a=1)");
        let span_name = span.sub(4..7);
        let span_args = span.sub(7..);
        let span_a = span.sub(8..9);
        let span_1 = span.sub(10..11);

        let node = NewExpressionNode::new(
            span,
            Some(PathNode::new(
                span_name,
                test_tokens!(Identifier:span_name),
                false,
            )),
            vec![ArgNode::Named(NamedArgNode::new(
                span_a + span_1,
                test_token!(Identifier:span_a),
                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                ))),
            ))]
            .spanned(span_args),
        );

        // act
        let expr = transform_new_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::new_(
                span,
                Path::name("Foo"),
                vec![Arg::named(
                    span_a + span_1,
                    ustr("a").spanned(span_a),
                    Expression::i32_literal(span_1, 1)
                )]
            )
        );
        errors.assert_empty();
    }

    #[test]
    fn shorthand_arg() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("new Foo(a)");
        let span_name = span.sub(4..7);
        let span_args = span.sub(7..);
        let span_a = span.sub(8..9);

        let node = NewExpressionNode::new(
            span,
            Some(PathNode::new(
                span_name,
                test_tokens!(Identifier:span_name),
                false,
            )),
            vec![ArgNode::Unnamed(UnnamedArgNode::new(
                ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span_a))),
            ))]
            .spanned(span_args),
        );

        // act
        let expr = transform_new_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::new_(
                span,
                Path::name("Foo"),
                vec![Arg::shorthand(
                    span_a,
                    ustr("a").spanned(span_a),
                    Expression::access(span_a, Path::name("a"))
                )]
            )
        );
        errors.assert_empty();
    }

    // todo: test error cases
}
