use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::type_node::{parse_type, type_starter, TypeNode};
use crate::parser::{consume_group, consume_token, expect_token, recover_until, Spanned};
use crate::source_map::Span;
use crate::token_starter;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{Colon, Comma, Identifier, OpenParen};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct FnParameterNode {
    pub span: Span,
    pub name: Token,
    pub type_: Option<TypeNode>,
}

pub fn parse_fn_parameters<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<Spanned<Vec<FnParameterNode>>> {
    let mut iter = iter.mark();
    let mut result = vec![];

    let (mut iter, group_span) = match consume_group(&mut iter, OpenParen) {
        Some(group) => (marking(group.children.iter()), group.span()),
        _ => {
            iter.reset();
            return None;
        }
    };

    token_starter!(identifier, Identifier);
    token_starter!(comma, Comma);
    token_starter!(colon, Colon);
    while recover_until(&mut iter, errors, [identifier], []) {
        let name = expect_token(&mut iter, Identifier);

        result.push(FnParameterNode {
            span: name.span,
            name: name,
            type_: None,
        });
        let param = result.last_mut().expect("literally just pushed");

        if !recover_until(&mut iter, errors, [colon, type_starter, comma], []) {
            break;
        }

        if let Some(colon_token) = consume_token(&mut iter, Colon) {
            param.span += colon_token.span;
        } else {
            errors.add(ErrorKind::MissingColon, param.span.end);
        }

        if !recover_until(&mut iter, errors, [type_starter, comma], []) {
            break;
        }

        if let Some(type_) = parse_type(&mut iter, errors) {
            param.span += type_.span();
            param.type_ = Some(type_);
        } else {
            errors.add(ErrorKind::MissingFunctionParameterType, param.span.end);
        }

        if !recover_until(&mut iter, errors, [comma, identifier], []) {
            break;
        }

        if consume_token(&mut iter, Comma).is_none() {
            errors.add(ErrorKind::MissingComma, param.span.end);
        }
    }

    Some(Spanned {
        span: group_span,
        value: result,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::errors::{ErrorKind, Errors};
    use crate::marking_iterator::marking;
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::NamedTypeNode;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokens, test_tokentree};

    #[test]
    fn parse_fn_parameter_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..19).into(),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..31).into(),
                value: vec![FnParameterNode {
                    span: (13..24).into(),
                    name: test_token!(Identifier:13..15),
                    type_: Some(TypeNode::Named(NamedTypeNode {
                        span: (18..24).into(),
                        path: PathNode {
                            span: (18..24).into(),
                            is_rooted: false,
                            parts: test_tokens!(Identifier:18..24),
                        },
                    }))
                }]
            })
        );
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..45).into(),
                value: vec![
                    FnParameterNode {
                        span: (13..24).into(),
                        name: test_token!(Identifier: 13..15),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (18..24).into(),
                            path: PathNode {
                                span: (18..24).into(),
                                is_rooted: false,
                                parts: test_tokens!(Identifier: 18..24),
                            },
                        }))
                    },
                    FnParameterNode {
                        span: (27..42).into(),
                        name: test_token!(Identifier: 27..33),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (37..42).into(),
                            path: PathNode {
                                span: (37..42).into(),
                                is_rooted: false,
                                parts: test_tokens!(Identifier: 37..42),
                            },
                        }))
                    }
                ]
            })
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_fn_parameter_missing_colon() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((:12, Identifier:13..15, Identifier:18..24, Comma:25, Identifier:27..33, Colon:34, Identifier:37..42):44);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..45).into(),
                value: vec![
                    FnParameterNode {
                        span: (13..24).into(),
                        name: test_token!(Identifier: 13..15),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (18..24).into(),
                            path: PathNode {
                                span: (18..24).into(),
                                parts: test_tokens!(Identifier: 18..24),
                                is_rooted: false,
                            },
                        })),
                    },
                    FnParameterNode {
                        span: (27..42).into(),
                        name: test_token!(Identifier: 27..33),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (37..42).into(),
                            path: PathNode {
                                span: (37..42).into(),
                                parts: test_tokens!(Identifier: 37..42),
                                is_rooted: false,
                            },
                        }))
                    }
                ]
            })
        );
        assert!(errors.has_error_at(15, ErrorKind::MissingColon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_missing_comma() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 12, Identifier: 13..15, Colon: 17, Identifier: 18..24, Identifier: 27..33, Colon: 34, Identifier: 37..42): 44);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..45).into(),
                value: vec![
                    FnParameterNode {
                        span: (13..24).into(),
                        name: test_token!(Identifier: 13..15),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (18..24).into(),
                            path: PathNode {
                                span: (18..24).into(),
                                parts: test_tokens!(Identifier: 18..24),
                                is_rooted: false,
                            },
                        })),
                    },
                    FnParameterNode {
                        span: (27..42).into(),
                        name: test_token!(Identifier: 27..33),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (37..42).into(),
                            path: PathNode {
                                span: (37..42).into(),
                                parts: test_tokens!(Identifier: 37..42),
                                is_rooted: false,
                            },
                        }))
                    }
                ]
            })
        );
        assert!(errors.has_error_at(24, ErrorKind::MissingComma));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_missing_type() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 12, Identifier: 13..15, Colon: 16, Comma: 25, Identifier: 27..33, Colon: 34, Identifier: 37..42): 44);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..45).into(),
                value: vec![
                    FnParameterNode {
                        span: (13..17).into(),
                        name: test_token!(Identifier: 13..15),
                        type_: None,
                    },
                    FnParameterNode {
                        span: (27..42).into(),
                        name: test_token!(Identifier: 27..33),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (37..42).into(),
                            path: PathNode {
                                span: (37..42).into(),
                                parts: test_tokens!(Identifier: 37..42),
                                is_rooted: false,
                            },
                        }))
                    }
                ]
            })
        );
        assert!(errors.has_error_at(17, ErrorKind::MissingFunctionParameterType));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_missing_colon_and_type() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 12, Identifier: 13..15, Comma: 25, Identifier: 27..33, Colon: 34, Identifier: 37..42): 44);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (12..45).into(),
                value: vec![
                    FnParameterNode {
                        span: (13..15).into(),
                        name: test_token!(Identifier: 13..15),
                        type_: None,
                    },
                    FnParameterNode {
                        span: (27..42).into(),
                        name: test_token!(Identifier: 27..33),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (37..42).into(),
                            path: PathNode {
                                span: (37..42).into(),
                                parts: test_tokens!(Identifier: 37..42),
                                is_rooted: false,
                            },
                        }))
                    }
                ]
            })
        );
        assert!(errors.has_error_at(15, ErrorKind::MissingFunctionParameterType));
        assert!(errors.has_error_at(15, ErrorKind::MissingColon));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_fn_parameter_unexpected_token() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((: 11, Unknown: 12, Identifier: 13..15, Unknown: 16, Colon: 17, Unknown: 18, Identifier: 19..24, Unknown: 25, Comma: 26, Unknown: 27, Identifier: 28..33, Colon: 35, Identifier: 37..42, Unknown: 43): 44);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(Spanned {
                span: (11..45).into(),
                value: vec![
                    FnParameterNode {
                        span: (13..24).into(),
                        name: test_token!(Identifier: 13..15),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (19..24).into(),
                            path: PathNode {
                                span: (19..24).into(),
                                is_rooted: false,
                                parts: test_tokens!(Identifier: 19..24),
                            },
                        }))
                    },
                    FnParameterNode {
                        span: (28..42).into(),
                        name: test_token!(Identifier: 28..33),
                        type_: Some(TypeNode::Named(NamedTypeNode {
                            span: (37..42).into(),
                            path: PathNode {
                                span: (37..42).into(),
                                is_rooted: false,
                                parts: test_tokens!(Identifier: 37..42),
                            },
                        }))
                    }
                ]
            })
        );
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
