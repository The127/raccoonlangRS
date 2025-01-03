use crate::parser::return_type_node::{parse_return_type, return_type_starter, ReturnTypeNode};
use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::MarkingIterator;
use crate::parser::{consume_token, recover_until, Spanned, Visibility};
use crate::parser::file_node::toplevel_starter;
use crate::source_map::Span;
use crate::{token_starter, group_starter};
use crate::parser::block_expression::{parse_block_expression};
use crate::parser::expression_node::ExpressionNode;
use crate::parser::fn_parameters::{parse_fn_parameters, FnParameterNode};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{Fn, Identifier, OpenCurly, OpenParen, Pub};
use crate::treeizer::TokenTree;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct FnNode {
    pub span: Span,
    pub visibility: Visibility,
    pub name: Option<Token>,
    pub parameters: Vec<FnParameterNode>,
    pub return_type: Option<ReturnTypeNode>,
    pub body: Option<ExpressionNode>,
}

pub fn parse_fn<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<FnNode>
{
    let mut result = FnNode::default();

    let mut mark = iter.mark();

    token_starter!(fn_starter, Fn);

    if let Some(pub_token) = consume_token(&mut mark, Pub) {
        result.span = pub_token.span;
        result.visibility = Visibility::Public(pub_token);

        let mut recover_errors = Errors::new();
        if !recover_until(&mut mark, &mut recover_errors, [fn_starter], [toplevel_starter]) {
            mark.reset();
            return None;
        }

        errors.merge(recover_errors);
    }

    if let Some(fn_token) = consume_token(&mut mark, Fn) {
        result.span += fn_token.span;
        mark.discard();
    } else {
        mark.reset();
        return None;
    };


    token_starter!(identifier, Identifier);
    group_starter!(param_starter, OpenParen);
    group_starter!(body_starter, OpenCurly);

    if !recover_until(iter, errors, [identifier, param_starter, return_type_starter, body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingFunctionName, result.span.end);
        errors.add(ErrorKind::MissingFunctionParameterList, result.span.end);
        errors.add(ErrorKind::MissingReturnType, result.span.end);
        errors.add(ErrorKind::MissingFunctionBody, result.span.end);
        return Some(result);
    }

    if let Some(name) = consume_token(iter, Identifier) {
        result.span += name.span;
        result.name = Some(name);
    } else {
        errors.add(ErrorKind::MissingFunctionName, result.span.end);
    }

    if !recover_until(iter, errors, [param_starter, return_type_starter, body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingFunctionParameterList, result.span.end);
        errors.add(ErrorKind::MissingReturnType, result.span.end);
        errors.add(ErrorKind::MissingFunctionBody, result.span.end);
        return Some(result);
    }

    if let Some(Spanned {span, value: params}) = parse_fn_parameters(iter, errors) {
        result.span += span;
        result.parameters = params;
    } else {
        errors.add(ErrorKind::MissingFunctionParameterList, result.span.end);
    }

    if !recover_until(iter, errors, [return_type_starter, body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingReturnType, result.span.end);
        errors.add(ErrorKind::MissingFunctionBody, result.span.end);
        return Some(result);
    }

    if let Some(return_type) = parse_return_type(iter, errors) {
        result.span += return_type.span;
        result.return_type = Some(return_type);
    } else {
        errors.add(ErrorKind:: MissingReturnType, result.span.end);
    }

    if !recover_until(iter, errors, [body_starter], [toplevel_starter]) {
        errors.add(ErrorKind::MissingReturnType, result.span.end);
        return Some(result);
    }

    if let Some(body) = parse_block_expression(iter, errors) {
        result.span += body.span;
        result.body = Some(ExpressionNode::Block(body));
    }

    Some(result)
}


#[cfg(test)]
mod test {
    use crate::marking_iterator::marking;
    use crate::{test_token, test_tokens, test_tokentree};
    use crate::errors::ErrorKind;
    use crate::parser::block_expression::BlockExpression;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression::{IntegerLiteral, LiteralExpression};
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::{NamedType, TypeNode};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_fn_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_trivial() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:12,):13, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
            parameters: vec![],
            return_type: Some(ReturnTypeNode{
                span: (14..19).into(),
                type_node: Some(TypeNode::Named(NamedType{
                    span: (16..19).into(),
                    path: PathNode{
                        span: (16..19).into(),
                        parts: test_tokens!(Identifier:16..19),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (19..21).into(),
                value: None,
            })),
        }));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_pub_fn() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:1..3, Fn:4..5, Identifier:6..10, (:12,):13, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (1..21).into(),
            visibility: Visibility::Public(test_token!(Pub:1..3)),
            name: Some(test_token!(Identifier:6..10)),
            parameters: vec![],
            return_type: Some(ReturnTypeNode{
                span: (14..19).into(),
                type_node: Some(TypeNode::Named(NamedType{
                    span: (16..19).into(),
                    path: PathNode{
                        span: (16..19).into(),
                        parts: test_tokens!(Identifier:16..19),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (19..21).into(),
                value: None,
            })),
        }));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_name() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, (:12,):13, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: None,
            parameters: vec![],
            return_type: Some(ReturnTypeNode{
                span: (14..19).into(),
                type_node: Some(TypeNode::Named(NamedType{
                    span: (16..19).into(),
                    path: PathNode{
                        span: (16..19).into(),
                        parts: test_tokens!(Identifier:16..19),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (19..21).into(),
                value: None,
            })),
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
            parameters: vec![],
            return_type: Some(ReturnTypeNode{
                span: (14..19).into(),
                type_node: Some(TypeNode::Named(NamedType{
                    span: (16..19).into(),
                    path: PathNode{
                        span: (16..19).into(),
                        parts: test_tokens!(Identifier:16..19),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (19..21).into(),
                value: None,
            })),
        }));
        assert!(errors.has_error_at(10, ErrorKind::MissingFunctionParameterList));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_return_type() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:10, ):11, {:15, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
            parameters: vec![],
            return_type: None,
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (15..21).into(),
                value: None,
            })),
        }));
        assert!(errors.has_error_at(12, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_return_type_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10, (:12,):13,);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..14).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
            parameters: vec![],
            return_type: None,
            body: None,
        }));
        assert!(errors.has_error_at(14, ErrorKind::MissingFunctionBody));
        assert!(errors.has_error_at(14, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_name_and_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, DashArrow:14..16, Identifier:16..19, {:19, }:20);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..21).into(),
            visibility: Visibility::Module,
            name: None,
            parameters: vec![],
            return_type: Some(ReturnTypeNode{
                span: (14..19).into(),
                type_node: Some(TypeNode::Named(NamedType{
                    span: (16..19).into(),
                    path: PathNode{
                        span: (16..19).into(),
                        parts: test_tokens!(Identifier:16..19),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (19..21).into(),
                value: None,
            })),
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionParameterList));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_name_and_return_type_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, (:12,):13);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..14).into(),
            visibility: Visibility::Module,
            name: None,
            parameters: vec![],
            return_type: None,
            body: None,
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
        assert!(errors.has_error_at(14, ErrorKind::MissingReturnType));
        assert!(errors.has_error_at(14, ErrorKind::MissingFunctionBody));
        assert_eq!(errors.get_errors().len(), 3);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_missing_params_and_return_type_and_body() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:6..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..10).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:6..10)),
            parameters: vec![],
            return_type: None,
            body: None,
        }));
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..5).into(),
            visibility: Visibility::Module,
            name: None,
            parameters: vec![],
            return_type: None,
            body: None,
        }));
        assert!(errors.has_error_at(5, ErrorKind::MissingFunctionName));
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..31).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:9..12)),
            parameters: vec![],
            return_type: Some(ReturnTypeNode{
                span: (19..24).into(),
                type_node: Some(TypeNode::Named(NamedType{
                    span: (21..24).into(),
                    path: PathNode{
                        span: (21..24).into(),
                        parts: test_tokens!(Identifier:21..24),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (28..31).into(),
                value: None,
            })),
        }));
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
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16,):17, DashArrow:19..21, {:28, }:30);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..31).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:9..12)),
            parameters: vec![],
            return_type: Some(ReturnTypeNode {
                span: (19..21).into(),
                type_node: None
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (28..31).into(),
                value: None,
            })),
        }));
        assert!(errors.has_error_at(21, ErrorKind::MissingReturnType));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_simple_body() {
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16,):17, DashArrow:19..21, Identifier:23..25, {:28, DecInteger:29..32 }:35);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..36).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:9..12)),
            parameters: vec![],
            return_type: Some(ReturnTypeNode {
                span: (19..25).into(),
                type_node: Some(TypeNode::Named(NamedType {
                    span: (23..25).into(),
                    path: PathNode {
                        span: (23..25).into(),
                        parts: test_tokens!(Identifier:23..25),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (28..36).into(),
                value: Some(Box::new(ExpressionNode::Literal(LiteralExpression::Integer(IntegerLiteral {
                    span: (29..32).into(),
                    number: test_token!(DecInteger:29..32),
                    negative: false,
                })))),
            })),
        }));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_with_params() {
        let input: Vec<TokenTree> = test_tokentree!(Fn:3..5, Identifier:9..12, (:16, Identifier:17..20, Colon:21, Identifier:22..25 ):26, DashArrow:28..30, Identifier:30..35, {:36, }:37);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(FnNode {
            span: (3..38).into(),
            visibility: Visibility::Module,
            name: Some(test_token!(Identifier:9..12)),
            parameters: vec![
                FnParameterNode {
                    span: (17..25).into(),
                    name: test_token!(Identifier:17..20),
                    type_: Some(TypeNode::Named(NamedType {
                        span: (22..25).into(),
                        path: PathNode {
                            span: (22..25).into(),
                            parts: test_tokens!(Identifier:22..25),
                            is_rooted: false,
                        },
                    })),
                }
            ],
            return_type: Some(ReturnTypeNode {
                span: (28..35).into(),
                type_node: Some(TypeNode::Named(NamedType {
                    span: (30..35).into(),
                    path: PathNode {
                        span: (30..35).into(),
                        parts: test_tokens!(Identifier:30..35),
                        is_rooted: false,
                    },
                }))
            }),
            body: Some(ExpressionNode::Block(BlockExpression {
                span: (36..38).into(),
                value: None,
            })),
        }));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }
}