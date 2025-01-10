use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::{ErrorKind, Errors};
use crate::parser::file_node::toplevel_starter;
use crate::parser::path_node::{parse_path, path_starter, PathNode};
use crate::parser::*;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType::*;
use crate::treeizer::*;
use crate::{consume_token, expect_token, token_starter};

#[derive(Debug, Eq, PartialEq, Default)]
pub struct UseNode {
    span_: Span,
    pub path: Option<PathNode>,
    pub alias: Option<Token>,
    pub multi: Option<Vec<MultiUseNode>>,
}

impl HasSpan for UseNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_use<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<UseNode> {
    let use_token = match consume_token!(iter, Use) {
        Some(use_token) => use_token,
        _ => return None,
    };

    let mut result = UseNode {
        span_: use_token.span(),
        ..UseNode::default()
    };

    if !recover_until(
        iter,
        errors,
        [path_starter, alias_starter, multi_use_starter, semicolon],
        [toplevel_starter],
    ) {
        errors.add(ErrorKind::MissingUsePath, result.span_.end());
        errors.add(ErrorKind::MissingSemicolon, result.span_.end());
        return Some(result);
    }

    if let Some(path) = parse_path(iter, errors) {
        result.span_ += path.span();
        result.path = Some(path);
    } else {
        errors.add(ErrorKind::MissingUsePath, result.span_.end());
    }

    if !recover_until(
        iter,
        errors,
        [alias_starter, multi_use_starter, semicolon],
        [toplevel_starter],
    ) {
        errors.add(ErrorKind::MissingSemicolon, result.span_.end());
        return Some(result);
    }

    if let Some(alias) = parse_use_alias(iter, errors) {
        result.span_ += alias.span();
        result.alias = alias.name;
    } else if let Some(multi) = parse_multi_use(iter, errors) {
        result.span_ += multi.span();
        result.multi = Some(multi.value);
    }

    if !recover_until(iter, errors, [semicolon], [toplevel_starter]) {
        errors.add(ErrorKind::MissingSemicolon, result.span_.end());
        return Some(result);
    }

    expect_token!(iter, Semicolon);

    Some(result)
}

#[derive(Default)]
struct Alias {
    span_: Span,
    name: Option<Token>,
}

impl HasSpan for Alias {
    fn span(&self) -> Span {
        self.span_
    }
}

token_starter!(alias_starter, As);
token_starter!(semicolon, Semicolon);
fn parse_use_alias<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<Alias> {
    let mut result = Alias::default();

    if let Some(as_token) = consume_token!(iter, As) {
        result.span_ = as_token.span();
    } else {
        return None;
    }

    token_starter!(identifier, Identifier);
    if !recover_until(iter, errors, [identifier], [semicolon, toplevel_starter]) {
        errors.add(ErrorKind::MissingUseAliasName, result.span_.end());
        return Some(result);
    }

    let name = expect_token!(iter, Identifier);
    result.name = Some(name);
    result.span_ += name.span();

    Some(result)
}

#[derive(Debug, Eq, PartialEq)]
pub struct MultiUseNode {
    span_: Span,
    pub name: Token, //TODO: this wants to be an option
    pub alias: Option<Token>,
}

fn multi_use_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    let mut mark = iter.mark().auto_reset();

    if consume_token!(&mut mark, PathSeparator).is_some() {
        match mark.next() {
            Some(TokenTree::Group(Group {
                open:
                    Token {
                        token_type: OpenCurly,
                        ..
                    },
                ..
            })) => return true,
            _ => (),
        };
    }
    false
}

fn parse_multi_use<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<Spanned<Vec<MultiUseNode>>> {
    let mut iter = iter.mark();

    if consume_token!(&mut iter, PathSeparator).is_none() {
        return None;
    }

    let group = match consume_group(&mut iter, OpenCurly) {
        Some(group) => group,
        _ => {
            iter.reset();
            return None;
        }
    };

    let mut iter = make_awesome(group.children.iter());

    let mut result: Spanned<Vec<MultiUseNode>> = Spanned {
        span_: group.span(),
        ..Spanned::default()
    };

    token_starter!(identifier, Identifier);
    token_starter!(comma, Comma);
    while recover_until(&mut iter, errors, [identifier, alias_starter], []) {
        if let Some(name) = consume_token!(&mut iter, Identifier) {
            result.value.push(MultiUseNode {
                name: name,
                span_: name.span(),
                alias: None,
            });
            let current = result
                .value
                .last_mut()
                .expect("literally just pushed a value...");
            if !recover_until(&mut iter, errors, [alias_starter, comma, identifier], []) {
                continue;
            }
            if let Some(alias) = parse_use_alias(&mut iter, errors) {
                current.span_ += alias.span_;
                current.alias = alias.name;
            }

            if !recover_until(&mut iter, errors, [comma, identifier], []) {
                continue;
            }

            if consume_token!(&mut iter, Comma).is_none() {
                errors.add(ErrorKind::MissingComma, current.span_.end());
            }

            continue;
        } else if let Some(alias) = parse_use_alias(&mut iter, errors) {
            errors.add(ErrorKind::MissingMultiUseName, alias.span_.start());
        }
    }

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use assert_matches::assert_matches;

    #[test]
    fn parse_use_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_not_a_use() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Mod, Identifier, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(iter.collect::<Vec<_>>(), input.iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_use_one_use() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, Identifier, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(
            result,
            Some(UseNode {
                span_: Span(0, 0),
                path: Some(_),
                alias: None,
                multi: None,
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_remaining() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, Identifier, Semicolon, Mod);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let _ = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            iter.collect::<Vec<_>>(),
            test_tokentree!(Mod).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_use_one_use_with_alias() {
        // arrange
        let input = test_tokentree!(
            Use,
            Identifier,
            PathSeparator,
            Identifier,
            As,
            Identifier,
            Semicolon
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(
            result,
            Some(UseNode {
                span_: Span(0, 0),
                path: Some(_),
                alias: Some(Token {
                    token_type: Identifier,
                    ..
                }),
                multi: None,
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_multiuse_one_without_alias() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, { Identifier }, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(0,0),
            path: Some(_),
            alias: None,
            multi: Some(multi),
        }) if matches!(multi[..], [
            MultiUseNode {
                span_: Span(0,0),
                name: Token {token_type: Identifier, ..},
                alias: None,
            }
        ]));
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_multiuse_two_without_alias() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, {Identifier, Comma, Identifier}, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(0,0),
            path: Some(_),
            alias: None,
            multi: Some(multi),
        }) if matches!(multi[..], [
            MultiUseNode {
                span_: Span(0,0),
                name: Token {token_type: Identifier, ..},
                alias: None,
            },
            MultiUseNode {
                span_: Span(0,0),
                name: Token {token_type: Identifier, ..},
                alias: None,
            },
        ]));
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_multiuse_two_with_alias() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, {Identifier, As, Identifier, Comma, Identifier, As, Identifier}, Semicolon);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(0,0),
            path: Some(_),
            alias: None,
            multi: Some(multi),
        }) if matches!(multi[..], [
            MultiUseNode {
                span_: Span(0,0),
                name: Token {token_type: Identifier, ..},
                alias: Some(Token {token_type: Identifier, ..}),
            },
            MultiUseNode {
                span_: Span(0,0),
                name: Token {token_type: Identifier, ..},
                alias: Some(Token {token_type: Identifier, ..}),
            }
        ]));
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_everything() {
        // arrange
        let input = test_tokentree!(Use:10..13);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span_: (10..13).into(),
                path: None,
                alias: None,
                multi: None,
            })
        );

        assert!(errors.has_error_at(13, ErrorKind::MissingUsePath));
        assert!(errors.has_error_at(13, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_use_path() {
        // arrange
        let input = test_tokentree!(Use:100..103, Semicolon:104);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span_: (100..103).into(),
                path: None,
                alias: None,
                multi: None,
            })
        );
        assert!(errors.has_error_at(103, ErrorKind::MissingUsePath));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_use_path_with_alias() {
        // arrange
        let input = test_tokentree!(Use:100..103, As:104..106, Identifier:107..110, Semicolon:110);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span_: (100..110).into(),
                path: None,
                alias: Some(test_token!(Identifier:107..110)),
                multi: None,
            })
        );
        assert!(errors.has_error_at(103, ErrorKind::MissingUsePath));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_semicolon() {
        // arrange
        let input =
            test_tokentree!(Use:10..13, Identifier:14..20, PathSeparator:20..22, Identifier:22..30);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(
            result,
            Some(UseNode {
                span_: Span(10, 30),
                path: Some(_),
                alias: None,
                multi: None,
            })
        );
        assert!(errors.has_error_at(30, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_semicolon_after_alias() {
        // arrange
        let input = test_tokentree!(Use:3..6, Identifier:7..10, PathSeparator:11..13, Identifier:13..20, As:21..23, Identifier:23..30);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(3, 30),
            path: Some(_),
            alias: Some(alias @ Token {token_type: Identifier, ..}),
            multi: None,
        }) if alias.span() == Span(23, 30));
        assert!(errors.has_error_at(30, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_semicolon_after_multi() {
        // arrange
        let input = test_tokentree!(Use:1..4, Identifier:5..10, PathSeparator:10..12, {:14, Identifier:16..20}:22);

        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(1, 23),
            path: Some(_),
            alias: None,
            multi: Some(multi),
        }) if matches!(multi[..], [
            MultiUseNode {
                span_: Span(16, 20),
                name: multi_token @ Token { token_type: Identifier, ..},
                alias: None,
            }
        ] if multi_token.span() == Span(16, 20)));
        assert!(errors.has_error_at(23, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_alias_name() {
        // arrange
        let input = test_tokentree!(Use:5..8, Identifier:10..13, PathSeparator:13..15, Identifier:15..20, As:21..23, Semicolon:24);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(
            result,
            Some(UseNode {
                span_: Span(5, 23),
                path: Some(_),
                alias: None,
                multi: None,
            })
        );
        assert!(errors.has_error_at(23, ErrorKind::MissingUseAliasName));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_alias_name_and_missing_semicolon() {
        // arrange
        let input = test_tokentree!(Use:0..3, Identifier:4..10, PathSeparator:10..12, Identifier:13..20, As:23..25);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(
            result,
            Some(UseNode {
                span_: Span(0, 25),
                path: Some(_),
                alias: None,
                multi: None,
            })
        );
        assert!(errors.has_error_at(25, ErrorKind::MissingUseAliasName));
        assert!(errors.has_error_at(25, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 2);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_comma_betwee_multiuse_items() {
        // arrange
        let input = test_tokentree!(Use:0..3, Identifier:4..10, PathSeparator:10..12, {:13, Identifier:14..20, As:23..25, Identifier:26..30, Identifier:31..40}:40, Semicolon:41);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(0, 41),
            path: Some(_),
            alias: None,
            multi: Some(multi),
        }) if matches!(multi[..], [
            MultiUseNode {
                span_: Span(14, 30),
                name: mt1 @ Token {token_type: Identifier, ..},
                alias: Some(ma1 @ Token {token_type: Identifier, ..})
            },
            MultiUseNode {
                span_: Span(31, 40),
                name: mt2 @ Token {token_type: Identifier, ..},
                alias: None
            },
        ] if mt1.span() == Span(14, 20)
            && ma1.span() == Span(26, 30)
            && mt2.span() == Span(31, 40)
        ));
        assert!(errors.has_error_at(30, ErrorKind::MissingComma));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_name_in_multi() {
        // arrange
        let input = test_tokentree!(Use:0..3, Identifier:4..10, PathSeparator:10..12, {:13, As:23..25, Identifier:26..30}:40, Semicolon:41);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_matches!(result, Some(UseNode {
            span_: Span(0, 41),
            path: Some(_),
            alias: None,
            multi: Some(multi),
        }) if multi.is_empty());
        assert!(errors.has_error_at(23, ErrorKind::MissingMultiUseName));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }
}
