use crate::awesome_iterator::AwesomeIterator;
use crate::errors::{ErrorKind, Errors};
use crate::parser::block_expression_node::parse_block_expression;
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::parser::file_node::toplevel_starter;
use crate::parser::fn_parameter_node::{parse_fn_parameters, FnParameterNode};
use crate::parser::return_type_node::{parse_return_type, return_type_starter, ReturnTypeNode};
use crate::parser::{recover_until, Spanned, Visibility};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;
use crate::{add_error, consume_token, expect_token, group_starter, token_starter};

#[derive(Debug, Default, Eq, PartialEq)]
pub struct FnNode {
    span_: Span,
    pub visibility: Visibility,
    pub name: Option<Token>,
    pub parameters: Vec<FnParameterNode>,
    pub return_type: Option<ReturnTypeNode>,
    pub body: Option<ExpressionNode>,
}

impl HasSpan for FnNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl FnNode {
    pub fn new<S: Into<Span>>(
        span: S,
        visibility: Visibility,
        name: Option<Token>,
        parameters: Vec<FnParameterNode>,
        return_type: Option<ReturnTypeNode>,
        body: Option<ExpressionNode>,
    ) -> Self {
        Self {
            span_: span.into(),
            visibility,
            name,
            parameters,
            return_type,
            body,
        }
    }
}

pub fn parse_fn<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<FnNode> {
    let mut result = FnNode::default();

    let mut mark = iter.mark();

    token_starter!(fn_starter, Fn);

    if let Some(pub_token) = consume_token!(&mut mark, Pub) {
        result.span_ = pub_token.span();
        result.visibility = Visibility::Public(pub_token);

        let mut recover_errors = Errors::new();
        if !recover_until(
            &mut mark,
            &mut recover_errors,
            [fn_starter],
            [toplevel_starter],
        ) {
            mark.reset();
            return None;
        }

        errors.merge(recover_errors);
    }

    if let Some(fn_token) = consume_token!(&mut mark, Fn) {
        result.span_ += fn_token.span();
        mark.discard();
    } else {
        mark.reset();
        return None;
    };

    token_starter!(identifier, Identifier);
    group_starter!(param_starter, OpenParen);
    group_starter!(body_starter, OpenCurly);
    token_starter!(lambda_starter, EqualArrow);

    if !recover_until(
        iter,
        errors,
        [identifier, param_starter, return_type_starter, body_starter, lambda_starter],
        [toplevel_starter],
    ) {
        add_error!(errors, result.span_.end(), MissingDeclarationName);
        add_error!(errors, result.span_.end(), MissingFunctionParameterList);
        add_error!(errors, result.span_.end(), MissingReturnType);
        add_error!(errors, result.span_.end(), MissingFunctionBody);
        return Some(result);
    }

    if let Some(name) = consume_token!(iter, Identifier) {
        result.span_ += name.span();
        result.name = Some(name);
    } else {
        add_error!(errors, result.span_.end(), MissingDeclarationName);
    }

    if !recover_until(
        iter,
        errors,
        [param_starter, return_type_starter, body_starter, lambda_starter],
        [toplevel_starter],
    ) {
        add_error!(errors, result.span_.end(), MissingFunctionParameterList);
        add_error!(errors, result.span_.end(), MissingReturnType);
        add_error!(errors, result.span_.end(), MissingFunctionBody);
        return Some(result);
    }

    if let Some(Spanned {
        span_: span,
        value: params,
    }) = parse_fn_parameters(iter, errors)
    {
        result.span_ += span;
        result.parameters = params;
    } else {
        add_error!(errors, result.span_.end(), MissingFunctionParameterList);
    }

    if !recover_until(
        iter,
        errors,
        [return_type_starter, body_starter, lambda_starter],
        [toplevel_starter],
    ) {
        add_error!(errors, result.span_.end(), MissingReturnType);
        add_error!(errors, result.span_.end(), MissingFunctionBody);
        return Some(result);
    }

    if let Some(return_type) = parse_return_type(iter, errors) {
        result.span_ += return_type.span();
        result.return_type = Some(return_type);
    } else {
        add_error!(errors, result.span_.end(), MissingReturnType);
    }

    if !recover_until(iter, errors, [body_starter, lambda_starter], [toplevel_starter]) {
        add_error!(errors, result.span_.end(), MissingReturnType);
        return Some(result);
    }

    if let Some(body) = parse_block_expression(iter, errors) {
        result.span_ += body.span();
        result.body = Some(body);
    } else {
        // only viable alternative is '=>' now
        let arrow_token = expect_token!(iter, EqualArrow);
        result.span_ += arrow_token.span();

        let body = parse_expression(iter, errors, false);
        result.span_ += body.span();
        result.body = body;

        token_starter!(semicolon, Semicolon);
        if !recover_until(iter, errors, [semicolon], [toplevel_starter]) {
            add_error!(errors, result.span_.end(), MissingSemicolon);
            return Some(result);
        }

        let semicolon = expect_token!(iter, Semicolon);
        result.span_ += semicolon.span();
    }

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind;
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{NumberLiteralNode, LiteralExpressionNode};
    use crate::parser::type_node::TypeNode;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use assert_matches::assert_matches;
    use crate::errors::ErrorKind::MissingSemicolon;

    #[test]
    fn parse_fn_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_trivial() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:12,):13, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 21),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if name.span() == Span(6, 10) && params.is_empty());
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_pub_fn() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:1..3, Fn:4..5, Identifier:6..10, (:12,):13, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(1,21),
            visibility: Visibility::Public(vis @ Token {token_type: Pub, .. }),
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if vis.span() == Span(1, 3) && name.span() == Span(6, 10) && params.is_empty());
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_name() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Fn:3..5, (:12,):13, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 21),
            visibility: Visibility::Module,
            name: None,
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if params.is_empty());
        assert!(errors.has_error_at(5, ErrorKind::MissingDeclarationName));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 21),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if name.span() == Span(6, 10) && params.is_empty());
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionParameterList));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_return_type() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Fn:3..5, Identifier:6..10, (:10, ):11, {:15, }:20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 21),
            visibility: Visibility::Module,
            name: Some(name @ Token { token_type: Identifier, .. }),
            parameters: params,
            return_type: None,
            body: Some(ExpressionNode::Block(_)),
        }) if name.span() == Span(6, 10) && params.is_empty());
        assert!(errors.has_error_at(12, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_return_type_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:12,):13,);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(FnNode {
                span_: (3..14).into(),
                visibility: Visibility::Module,
                name: Some(test_token!(Identifier:6..10)),
                parameters: vec![],
                return_type: None,
                body: None,
            })
        );
        assert!(errors.has_error_at(14, ErrorKind::MissingFunctionBody));
        assert!(errors.has_error_at(14, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_name_and_params() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Fn:3..5, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 21),
            visibility: Visibility::Module,
            name: None,
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if params.is_empty());
        assert!(errors.has_error_at(5, ErrorKind::MissingDeclarationName));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionParameterList));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_name_and_return_type_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, (:12,):13);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(FnNode {
                span_: (3..14).into(),
                visibility: Visibility::Module,
                name: None,
                parameters: vec![],
                return_type: None,
                body: None,
            })
        );
        assert!(errors.has_error_at(5, ErrorKind::MissingDeclarationName));
        assert!(errors.has_error_at(14, ErrorKind::MissingReturnType));
        assert!(errors.has_error_at(14, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 3);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_params_and_return_type_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(FnNode {
                span_: (3..10).into(),
                visibility: Visibility::Module,
                name: Some(test_token!(Identifier:6..10)),
                parameters: vec![],
                return_type: None,
                body: None,
            })
        );
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionParameterList));
        assert!(errors.has_error_at(10, ErrorKind::MissingReturnType));
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 3);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_everything() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(FnNode {
                span_: (3..5).into(),
                visibility: Visibility::Module,
                name: None,
                parameters: vec![],
                return_type: None,
                body: None,
            })
        );
        assert!(errors.has_error_at(5, ErrorKind::MissingDeclarationName));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionParameterList));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionBody));
        assert!(errors.has_error_at(5, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 4);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_unexpected_tokens() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Unknown:6..8, Identifier:9..12, Unknown:13..15, (:16,):17, Unknown:18, DashArrow:19..21, Identifier:21..24, Unknown:25..27, {:28, }:30);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 31),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if name.span() == Span(9, 12) && params.is_empty());
        assert!(errors.has_error_at(6..8, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(13..15, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(13..15, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(18, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(25..27, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 4);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_return_type_after_dasharrow() {
        let input: Vec<TokenTree> =
            test_tokentree!(Fn:3..5, Identifier:9..12, (:16,):17, DashArrow:19..21, {:28, }:30);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 31),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(ret @ ReturnTypeNode {
                type_node: None,
                ..
            }),
            body: Some(ExpressionNode::Block(_)),
        }) if name.span() == Span(9, 12) && params.is_empty() && ret.span() == Span(19, 21));
        assert!(errors.has_error_at(21, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_simple_body() {
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16,):17, DashArrow:19..21, Identifier:23..25, {:28, DecInteger:29..32 }:35);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 36),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(BlockExpressionNode {
                value: Some(body),
                ..
            }))
        }) => {
            assert_eq!(name.span(), Span(9, 12));
            assert!(params.is_empty());
            assert_matches!(*body, ExpressionNode::Literal(LiteralExpressionNode::Number(NumberLiteralNode {
                number: Token {token_type: DecInteger, ..},
                negative: false,
                ..
            })));
        });
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_lambda_body() {
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16,):17, DashArrow:19..21, Identifier:23..25, EqualArrow:27..29, DecInteger:29..32, Semicolon:33);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 34),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Literal(_))
        }) => {
            assert_eq!(name.span(), Span(9, 12));
            assert!(params.is_empty());
        });
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_lambda_body_missing_semicolon() {
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16,):17, DashArrow:19..21, Identifier:23..25, EqualArrow:27..29, DecInteger:29..32,);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 32),
            visibility: Visibility::Module,
            name: Some(name @ Token {token_type: Identifier, ..}),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Literal(_))
        }) => {
            assert_eq!(name.span(), Span(9, 12));
            assert!(params.is_empty());
        });
        errors.has_error_at(32, MissingSemicolon);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_with_params() {
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16, Identifier:17..20, Colon:21, Identifier:22..25 ):26, DashArrow:28..30, Identifier:30..35, {:36, }:37);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(FnNode {
            span_: Span(3, 38),
            visibility: Visibility::Module,
            name: Some(Token { token_type: Identifier, .. }),
            parameters: params,
            return_type: Some(_),
            body: Some(ExpressionNode::Block(_)),
        }) if matches!(&params[..], [
            param @ FnParameterNode {
                name: name @ Token {token_type: Identifier, .. },
                type_: Some(TypeNode::Named(_)),
                ..
            }
        ] if param.span() == Span(17, 25) && name.span() == Span(17, 20)));
        errors.assert_empty();
        assert!(remaining.is_empty());
    }
}
