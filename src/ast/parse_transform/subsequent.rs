use crate::add_error;
use crate::ast::expressions::arg::Arg;
use crate::ast::parse_transform::{transform_expression, Expression};
use crate::errors::Errors;
use crate::parser::arg_node::{ArgNode, NamedArgNode};
use crate::parser::expression_node::ExpressionNode;
use crate::parser::subsequent_expression_node::{CallLikeType, SubsequentExpressionFollowNode, SubsequentExpressionNode};
use crate::parser::ToSpanned;
use crate::source_map::{HasSpan, SourceCollection};

fn map_named_arg(node: &NamedArgNode, errors: &mut Errors, sources: &SourceCollection) -> Arg {
    let value = if let Some(value) = node.value.as_ref() {
        transform_expression(value, errors, sources)
    } else {
        Expression::unknown()
    };
    Arg::named(
        node.span(),
        sources
            .get_identifier(node.name.span())
            .spanned(node.name.span()),
        value,
    )
}

pub fn transform_subsequent_expression(
    node: &SubsequentExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    let mut result = transform_expression(&node.left, errors, sources);

    for follow in &node.follows {
        result = match follow {
            SubsequentExpressionFollowNode::CallLike(call_like) => match call_like.type_ {
                CallLikeType::Call => Expression::call(
                    result.span() + call_like.span(),
                    result,
                    call_like
                        .args
                        .iter()
                        .map(|arg| match arg {
                            ArgNode::Named(named) => map_named_arg(named, errors, sources),
                            ArgNode::Unnamed(positional) => Arg::Unnamed(transform_expression(
                                positional.value.as_ref(),
                                errors,
                                sources,
                            )),
                        })
                        .collect(),
                ),
                CallLikeType::Index => Expression::index(
                    result.span() + call_like.span(),
                    result,
                    call_like
                        .args
                        .iter()
                        .map(|arg| match arg {
                            ArgNode::Unnamed(x) => {
                                transform_expression(x.value.as_ref(), errors, sources)
                            }
                            ArgNode::Named(x) => {
                                add_error!(errors, x.name.span(), NamedArgumentNotAllowed);
                                if let Some(value) = x.value.as_ref() {
                                    transform_expression(value, errors, sources)
                                } else {
                                    Expression::unknown()
                                }
                            }
                        })
                        .collect(),
                ),
                CallLikeType::With => Expression::with(
                    result.span() + call_like.span(),
                    result,
                    call_like
                        .args
                        .iter()
                        .filter_map(|arg| match arg {
                            ArgNode::Named(named) => Some(map_named_arg(named, errors, sources)),
                            ArgNode::Unnamed(unnamed) => match unnamed.value.as_ref() {
                                ExpressionNode::Access(access) => Some(Arg::shorthand(
                                    unnamed.span(),
                                    sources
                                        .get_identifier(access.identifier.span())
                                        .spanned(unnamed.span()),
                                    transform_expression(unnamed.value.as_ref(), errors, sources),
                                )),
                                _ => {
                                    add_error!(
                                        errors,
                                        unnamed.span(),
                                        PositionalArgumentNotAllowed
                                    );
                                    None
                                }
                            },
                        })
                        .collect(),
                ),
            },
            SubsequentExpressionFollowNode::DotAccess(dot_access) => Expression::dot_access(
                result.span() + dot_access.span(),
                result,
                sources
                    .get_identifier(dot_access.name.span())
                    .spanned(dot_access.name.span()),
            ),
        };
    }

    result
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::arg::Arg;
    use crate::ast::parse_transform::{transform_expression, Expression};
    use crate::ast::path::Path;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{LiteralExpressionNode, NumberLiteralNode};
    use crate::parser::subsequent_expression_node::{
        SubsequentCallLikeNode, SubsequentDotAccessNode,
        SubsequentExpressionFollowNode, SubsequentExpressionNode,
    };
    use crate::parser::ToSpanned;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::{DecInteger, Identifier};
    use ustr::ustr;
    use crate::parser::arg_node::{ArgNode, NamedArgNode, UnnamedArgNode};

    #[test]
    fn dot_access_follow() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a.b");
        let span_a = span.sub(0..1);
        let span_dot = span.sub(1..2);
        let span_b = span.sub(2..3);

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::DotAccess(
                SubsequentDotAccessNode::new(span_dot + span_b, test_token!(Identifier:span_b)),
            )],
        ));

        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::dot_access(
                span,
                Expression::access(span_a, Path::name("a")),
                ustr("b").spanned(span_b)
            )
        );
        errors.assert_empty();
    }

    #[test]
    fn call_follow() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a(b, c, d=1, e=2, f, g)");
        let span_a = span.sub(0..1);
        let span_parens = span.sub(1..);
        let span_b = span.sub(2..3);
        let span_c = span.sub(5..6);
        let span_d = span.sub(8..9);
        let span_1 = span.sub(10..11);
        let span_e = span.sub(13..14);
        let span_2 = span.sub(15..16);
        let span_f = span.sub(18..19);
        let span_g = span.sub(21..22);

        let unnamed = |span| {
            ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Access(
                AccessExpressionNode::new(test_token!(Identifier:span)),
            )))
        };

        let named = |span_name, span_value| {
            ArgNode::Named(NamedArgNode::new(
                span_name + span_value,
                test_token!(Identifier:span_name),
                Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                    NumberLiteralNode::new(span_value, test_token!(DecInteger:span_value), false),
                ))),
            ))
        };

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::call(
                    span_parens,
                    vec![
                        unnamed(span_b),
                        unnamed(span_c),
                        named(span_d, span_1),
                        named(span_e, span_2),
                        unnamed(span_f),
                        unnamed(span_g),
                    ],
                ),
            )],
        ));

        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::call(
                span,
                Expression::access(span_a, Path::name("a")),
                vec![
                    Arg::unnamed(Expression::access(span_b, Path::name("b"))),
                    Arg::unnamed(Expression::access(span_c, Path::name("c"))),
                    Arg::named(
                        span_d + span_1,
                        ustr("d").spanned(span_d),
                        Expression::i32_literal(span_1, 1)
                    ),
                    Arg::named(
                        span_e + span_2,
                        ustr("e").spanned(span_e),
                        Expression::i32_literal(span_2, 2)
                    ),
                    Arg::unnamed(Expression::access(span_f, Path::name("f"))),
                    Arg::unnamed(Expression::access(span_g, Path::name("g"))),
                ]
            )
        );
        errors.assert_empty();
    }

    #[test]
    fn call_follow_named_arg_missing_value() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a(b=)");
        let span_a = span.sub(0..1);
        let span_parens = span.sub(1..);
        let span_b = span.sub(2..3);
        let span_eq = span.sub(3..4);

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::call(
                    span_parens,
                    vec![ArgNode::Named(NamedArgNode::new(
                        span_b + span_eq,
                        test_token!(Identifier:span_b),
                        None,
                    ))],
                ),
            )],
        ));

        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::call(
                span,
                Expression::access(span_a, Path::name("a")),
                vec![Arg::named(
                    span_b + span_eq,
                    ustr("b").spanned(span_b),
                    Expression::unknown()
                ),]
            )
        );
        errors.assert_empty();
    }

    #[test]
    fn index_follow() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a[b,2,3]");
        let span_a = span.sub(0..1);
        let span_parens = span.sub(1..);
        let span_b = span.sub(2..3);
        let span_2 = span.sub(4..5);
        let span_3 = span.sub(6..7);

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::index(
                    span_parens,
                    vec![
                        ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Access(
                            AccessExpressionNode::new(test_token!(Identifier:span_b)),
                        ))),
                        ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Literal(
                            LiteralExpressionNode::Number(NumberLiteralNode::new(
                                span_2,
                                test_token!(DecInteger:span_2),
                                false,
                            )),
                        ))),
                        ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Literal(
                            LiteralExpressionNode::Number(NumberLiteralNode::new(
                                span_3,
                                test_token!(DecInteger:span_3),
                                false,
                            )),
                        ))),
                    ],
                ),
            )],
        ));
        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::index(
                span,
                Expression::access(span_a, Path::name("a")),
                vec![
                    Expression::access(span_b, Path::name("b")),
                    Expression::i32_literal(span_2, 2),
                    Expression::i32_literal(span_3, 3),
                ]
            )
        );
        errors.assert_empty();
    }

    #[test]
    fn index_follow_named_not_allowed() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a[b=1,c=]");
        let span_a = span.sub(0..1);
        let span_parens = span.sub(1..);
        let span_b = span.sub(2..3);
        let span_1 = span.sub(4..5);
        let span_c = span.sub(6..7);
        let span_eq = span.sub(8..9);

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::index(
                    span_parens,
                    vec![
                        ArgNode::Named(NamedArgNode::new(
                            span_b + span_1,
                            test_token!(Identifier:span_b),
                            Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                NumberLiteralNode::new(
                                    span_1,
                                    test_token!(DecInteger:span_1),
                                    false,
                                ),
                            ))),
                        )),
                        ArgNode::Named(NamedArgNode::new(
                            span_c + span_eq,
                            test_token!(Identifier:span_c),
                            None,
                        )),
                    ],
                ),
            )],
        ));
        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::index(
                span,
                Expression::access(span_a, Path::name("a")),
                vec![Expression::i32_literal(span_1, 1), Expression::unknown(),]
            )
        );
        assert!(errors.has_error_at(span_b, ErrorKind::NamedArgumentNotAllowed));
        assert!(errors.has_error_at(span_c, ErrorKind::NamedArgumentNotAllowed));
        assert_eq!(errors.get_errors().len(), 2);
    }

    #[test]
    fn with_follow() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a with (b, c=1)");
        let span_a = span.sub(0..1);
        let span_with = span.sub(2..);
        let span_b = span.sub(8..9);
        let span_c = span.sub(11..12);
        let span_1 = span.sub(13..14);

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::with(
                    span_with,
                    vec![
                        ArgNode::Unnamed(UnnamedArgNode::new(ExpressionNode::Access(
                            AccessExpressionNode::new(test_token!(Identifier:span_b)),
                        ))),
                        ArgNode::Named(NamedArgNode::new(
                            span_c + span_1,
                            test_token!(Identifier:span_c),
                            Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
                                NumberLiteralNode::new(
                                    span_1,
                                    test_token!(DecInteger:span_1),
                                    false,
                                ),
                            ))),
                        )),
                    ],
                ),
            )],
        ));

        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::with(
                span,
                Expression::access(span_a, Path::name("a")),
                vec![
                    Arg::shorthand(
                        span_b,
                        ustr("b").spanned(span_b),
                        Expression::access(span_b, Path::name("b"))
                    ),
                    Arg::named(
                        span_c + span_1,
                        ustr("c").spanned(span_c),
                        Expression::i32_literal(span_1, 1)
                    ),
                ]
            )
        );
        errors.assert_empty();
    }

    #[test]
    fn with_follow_invalid_shorthand() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("a with (1)");
        let span_a = span.sub(0..1);
        let span_with = span.sub(2..);
        let span_1 = span.sub(8..9);

        let node = ExpressionNode::Subsequent(SubsequentExpressionNode::new(
            span,
            Box::new(ExpressionNode::Access(AccessExpressionNode::new(
                test_token!(Identifier:span_a),
            ))),
            vec![SubsequentExpressionFollowNode::CallLike(
                SubsequentCallLikeNode::with(
                    span_with,
                    vec![ArgNode::Unnamed(UnnamedArgNode::new(
                        ExpressionNode::Literal(LiteralExpressionNode::Number(
                            NumberLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
                        )),
                    ))],
                ),
            )],
        ));

        // act
        let expr = transform_expression(&node, &mut errors, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::with(span, Expression::access(span_a, Path::name("a")), vec![])
        );
        assert!(errors.has_error_at(span_1, ErrorKind::PositionalArgumentNotAllowed));
    }
}
