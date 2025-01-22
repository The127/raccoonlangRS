use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::{ErrorKind, Errors};
use crate::parser::type_node::{parse_type, type_starter, TypeNode};
use crate::parser::{consume_group, recover_until, Spanned};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::OpenParen;
use crate::treeizer::TokenTree;
use crate::{add_error, consume_token, expect_token, token_starter};

#[derive(Debug, Eq, PartialEq)]
pub struct FnParameterNode {
    span_: Span,
    pub name: Token,
    pub type_: Option<TypeNode>,
}

impl HasSpan for FnParameterNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl FnParameterNode {
    pub fn new<S: Into<Span>>(span: S, name: Token, type_: Option<TypeNode>) -> Self {
        Self {
            span_: span.into(),
            name,
            type_
        }
    }
}

pub fn parse_fn_parameters<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<Spanned<Vec<FnParameterNode>>> {
    let mut iter = iter.mark();
    let mut result = vec![];

    let (mut iter, group_span) = match consume_group(&mut iter, OpenParen) {
        Some(group) => (make_awesome(group.children.iter()), group.span()),
        _ => {
            iter.reset();
            return None;
        }
    };

    token_starter!(identifier, Identifier);
    token_starter!(comma, Comma);
    token_starter!(colon, Colon);
    while recover_until(&mut iter, errors, [identifier], []) {
        let name = expect_token!(&mut iter, Identifier);

        result.push(FnParameterNode {
            span_: name.span(),
            name: name,
            type_: None,
        });
        let param = result.last_mut().expect("literally just pushed");

        if !recover_until(&mut iter, errors, [colon, type_starter, comma], []) {
            break;
        }

        if let Some(colon_token) = consume_token!(&mut iter, Colon) {
            param.span_ += colon_token.span();
        } else {
            add_error!(errors, param.span_.end(), MissingColon);
        }

        if !recover_until(&mut iter, errors, [type_starter, comma], []) {
            break;
        }

        if let Some(type_) = parse_type(&mut iter, errors) {
            param.span_ += type_.span();
            param.type_ = Some(type_);
        } else {
            add_error!(errors, param.span_.end(), MissingFunctionParameterType);
        }

        if !recover_until(&mut iter, errors, [comma, identifier], []) {
            break;
        }

        if consume_token!(&mut iter, Comma).is_none() {
            add_error!(errors, param.span_.end(), MissingComma);
        }
    }

    Some(Spanned {
        span_: group_span,
        value: result,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::type_node::NamedTypeNode;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::test_tokentree;
    use assert_matches::assert_matches;

    #[test]
    fn parse_fn_parameter_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_fn_parameter_empty_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((:12, ):18);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span_: (12..19).into(),
                value: vec![]
            })
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_fn_parameter_single_param() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:12, Identifier:13..15, Colon:16, Identifier:18..24):30, Unknown:31);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(Spanned {
            value,
            ..
        }) if matches!(value[..], [
            FnParameterNode {
                type_: Some(TypeNode::Named(_)),
                name: Token {
                    token_type: Identifier,
                    ..
                },
                ..
            }
        ]));
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Unknown:31).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_fn_parameter_two_params() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((:12, Identifier: 13..15, Colon: 16, Identifier: 18..24, Comma: 25, Identifier: 27..33, Colon: 34, Identifier: 37..42):44);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(Spanned {
            value,
            ..
        }) if matches!(value[..], [
            FnParameterNode {
                type_: Some(TypeNode::Named(_)),
                name: Token {
                    token_type: Identifier,
                    ..
                },
                ..
            },
            FnParameterNode {

                type_: Some(TypeNode::Named(_)),
                name: Token {
                    token_type: Identifier,
                    ..
                },
                ..
            },
        ]));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_fn_parameter_missing_colon() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((:12, Identifier:13..15, Identifier:18..24, Comma:25, Identifier:27..33, Colon:34, Identifier:37..42):44);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().span(), Span(12, 45));
        assert_matches!(
            result.unwrap().value[..],
            [
                FnParameterNode {
                    span_: Span(13, 24),
                    name: Token {
                        token_type: Identifier,
                        ..
                    },
                    type_: Some(TypeNode::Named(NamedTypeNode { .. })),
                },
                FnParameterNode {
                    span_: Span(27, 42),
                    name: Token {
                        token_type: Identifier,
                        ..
                    },
                    type_: Some(TypeNode::Named(NamedTypeNode { .. }))
                }
            ]
        );
        assert!(errors.has_error_at(15, ErrorKind::MissingColon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_missing_comma() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 12, Identifier: 13..15, Colon: 17, Identifier: 18..24, Identifier: 27..33, Colon: 34, Identifier: 37..42): 44);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().span(), Span(12, 45));
        assert_matches!(
            result.unwrap().value[..],
            [
                FnParameterNode {
                    span_: Span(13, 24),
                    name: Token {
                        token_type: Identifier,
                        ..
                    },
                    type_: Some(TypeNode::Named(_)),
                    ..
                },
                FnParameterNode {
                    span_: Span(27, 42),
                    name: Token {
                        token_type: Identifier,
                        ..
                    },
                    type_: Some(TypeNode::Named(_)),
                    ..
                },
            ]
        );
        assert!(errors.has_error_at(24, ErrorKind::MissingComma));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_missing_type() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 12, Identifier: 13..15, Colon: 16, Comma: 25, Identifier: 27..33, Colon: 34, Identifier: 37..42): 44);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().span(), Span(12, 45));
        assert_matches!(result.unwrap().value[..], [
            FnParameterNode {
                span_: Span(13, 17),
                name: Token {token_type: Identifier, ..},
                type_: None,
            },
            FnParameterNode {
                span_: Span(27,42),
                name: Token {token_type: Identifier, ..},
                type_: Some(TypeNode::Named(_)),
            },
        ]);
        assert!(errors.has_error_at(17, ErrorKind::MissingFunctionParameterType));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_missing_colon_and_type() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 12, Identifier: 13..15, Comma: 25, Identifier: 27..33, Colon: 34, Identifier: 37..42): 44);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().span(), Span(12, 45));
        assert_matches!(result.unwrap().value[..], [
            FnParameterNode {
                span_: Span(13, 15),
                name: Token {token_type: Identifier, ..},
                type_: None,
            },
            FnParameterNode {
                span_: Span(27, 42),
                name: Token {token_type: Identifier, ..},
                type_: Some(TypeNode::Named(_)),
            }
        ]);
        assert!(errors.has_error_at(15, ErrorKind::MissingFunctionParameterType));
        assert!(errors.has_error_at(15, ErrorKind::MissingColon));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_unexpected_token() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 11, Unknown: 12, Identifier: 13..15, Unknown: 16, Colon: 17, Unknown: 18, Identifier: 19..24, Unknown: 25, Comma: 26, Unknown: 27, Identifier: 28..33, Colon: 35, Identifier: 37..42, Unknown: 43): 44);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().span(), Span(11, 45));
        assert_matches!(result.unwrap().value[..], [
            FnParameterNode {
                span_: Span(13, 24),
                name: Token {token_type: Identifier, ..},
                type_: Some(TypeNode::Named(_)),
            },
            FnParameterNode {
                span_: Span(28,42),
                name: Token {token_type: Identifier, ..},
                type_: Some(TypeNode::Named(_)),
            },
        ]);
        assert!(errors.has_error_at(12, UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(16, UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(18, UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(25, UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(27, UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(43, UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 6);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
