use crate::errors::Errors;
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::type_node::{parse_type, type_starter, TypeNode};
use crate::parser::{consume_group, consume_token, expect_token, recover_until};
use crate::source_map::Span;
use crate::token_starter;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{Colon, Comma, Identifier, OpenParen};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct FnParameterNode {
    span: Span,
    name: Token,
    ttype: Option<TypeNode>,
}

pub fn parse_fn_parameters<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<Vec<FnParameterNode>> {
    let mut iter = iter.mark();
    let mut result = vec![];

    let mut iter = match consume_group(&mut iter, OpenParen) {
        Some(group) => marking(group.children.iter()),
        _ => {
            iter.reset();
            return None;
        }
    };

    token_starter!(identifier, Identifier);
    token_starter!(comma, Comma);
    while recover_until(&mut iter, errors, [identifier], []) {
        let name = expect_token(&mut iter, Identifier);

        result.push(FnParameterNode {
            span: name.span,
            name: name,
            ttype: None,
        });
        let mut param = result.last_mut().expect("literally just pushed");

        token_starter!(colon, Colon);
        if !recover_until(&mut iter, errors, [colon, type_starter], []) {
            break;
        }

        if let Some(colon_token) = consume_token(&mut iter, Colon) {
            param.span += colon_token.span;
        } else {
            // TODO: error
        }

        if !recover_until(&mut iter, errors, [type_starter], []) {
            break;
        }

        if let Some(ttype) = parse_type(&mut iter, errors) {
            param.span += ttype.span();
            param.ttype = Some(ttype);
        }

        _ = consume_token(&mut iter, Comma);
    }

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::{test_token, test_tokens, test_tokentree};
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::NamedType;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;

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
        assert_eq!(result, Some(vec!()));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_fn_parameter_single_param() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:12, Identifier:13..15, Colon:16, Identifier:18..24):30);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(vec![FnParameterNode {
                span: (13..24).into(),
                name: test_token!(Identifier:13..15),
                ttype: Some(TypeNode::Named(NamedType{
                    span: (18..24).into(),
                    path: PathNode{
                        span: (18..24).into(),
                        is_rooted: false,
                        parts: test_tokens!(Identifier:18..24),
                    },
                }))
            }])
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_fn_parameter_two_params() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:12, Identifier:13..15, Colon:16, Identifier:18..24, Comma:25, Identifier:27..33, Colon:34, Identifier:37..42):44);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_fn_parameters(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(vec![FnParameterNode {
                span: (13..24).into(),
                name: test_token!(Identifier:13..15),
                ttype: Some(TypeNode::Named(NamedType{
                    span: (18..24).into(),
                    path: PathNode{
                        span: (18..24).into(),
                        is_rooted: false,
                        parts: test_tokens!(Identifier:18..24),
                    },
                }))
            },FnParameterNode {
                span: (27..42).into(),
                name: test_token!(Identifier:27..33),
                ttype: Some(TypeNode::Named(NamedType{
                    span: (37..42).into(),
                    path: PathNode{
                        span: (37..42).into(),
                        is_rooted: false,
                        parts: test_tokens!(Identifier:37..42),
                    },
                }))
            }])
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
