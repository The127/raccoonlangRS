use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::file_node::toplevel_starter;
use crate::parser::path_node::{parse_path, path_starter, PathNode};
use crate::parser::*;
use crate::source_map::Span;
use crate::token_starter;
use crate::tokenizer::TokenType::*;
use crate::treeizer::*;

#[derive(Debug, Eq, PartialEq, Default)]
pub struct UseNode {
    pub span: Span,
    pub path: Option<PathNode>,
    pub alias: Option<Token>,
    pub multi: Option<Vec<MultiUseNode>>,
}

pub fn parse_use<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<UseNode> {
    let use_token = match consume_token(iter, Use) {
        Some(use_token) => use_token,
        _ => return None,
    };

    let mut result = UseNode {
        span: use_token.span,
        ..UseNode::default()
    };

    if !recover_until(
        iter,
        errors,
        [path_starter, alias_starter, multi_use_starter, semicolon],
        [toplevel_starter],
    ) {
        errors.add(ErrorKind::MissingUsePath, result.span.end);
        errors.add(ErrorKind::MissingSemicolon, result.span.end);
        return Some(result);
    }

    if let Some(path) = parse_path(iter, errors) {
        result.span += path.span;
        result.path = Some(path);
    } else {
        errors.add(ErrorKind::MissingUsePath, result.span.end);
    }

    if !recover_until(
        iter,
        errors,
        [alias_starter, multi_use_starter, semicolon],
        [toplevel_starter],
    ) {
        errors.add(ErrorKind::MissingSemicolon, result.span.end);
        return Some(result);
    }

    if let Some(alias) = parse_use_alias(iter, errors) {
        result.span += alias.span;
        result.alias = alias.name;
    } else if let Some(multi) = parse_multi_use(iter, errors) {
        result.span += multi.span;
        result.multi = Some(multi.value);
    }

    if !recover_until(iter, errors, [semicolon], [toplevel_starter]) {
        errors.add(ErrorKind::MissingSemicolon, result.span.end);
        return Some(result);
    }

    expect_token(iter, Semicolon);

    Some(result)
}

#[derive(Default)]
struct Alias {
    span: Span,
    name: Option<Token>,
}

token_starter!(alias_starter, As);
token_starter!(semicolon, Semicolon);
fn parse_use_alias<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<Alias> {
    let mut result = Alias::default();

    if let Some(as_token) = consume_token(iter, As) {
        result.span = as_token.span;
    } else {
        return None;
    }

    token_starter!(identifier, Identifier);
    if !recover_until(iter, errors, [identifier], [semicolon, toplevel_starter]) {
        errors.add(ErrorKind::MissingUseAliasName, result.span.end);
        return Some(result);
    }

    let name = expect_token(iter, Identifier);
    result.name = Some(name);
    result.span += name.span;

    Some(result)
}

#[derive(Debug, Eq, PartialEq)]
pub struct MultiUseNode {
    pub span: Span,
    pub name: Token, //TODO: this wants to be an option
    pub alias: Option<Token>,
}

fn multi_use_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
) -> bool {
    let mut mark = iter.mark().auto_reset();

    if consume_token(&mut mark, PathSeparator).is_some() {
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
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<Spanned<Vec<MultiUseNode>>> {
    let mut iter = iter.mark();

    if consume_token(&mut iter, PathSeparator).is_none() {
        return None;
    }

    let group = match consume_group(&mut iter, OpenCurly) {
        Some(group) => group,
        _ => {
            iter.reset();
            return None;
        }
    };

    let mut iter = marking(group.children.iter());

    let mut result: Spanned<Vec<MultiUseNode>> = Spanned {
        span: group.span(),
        ..Spanned::default()
    };

    token_starter!(identifier, Identifier);
    token_starter!(comma, Comma);
    while recover_until(&mut iter, errors, [identifier, alias_starter], []) {
        if let Some(name) = consume_token(&mut iter, Identifier) {
            result.value.push(MultiUseNode {
                name: name,
                span: name.span,
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
                current.span += alias.span;
                current.alias = alias.name;
            }

            if !recover_until(&mut iter, errors, [comma, identifier], []) {
                continue;
            }

            if consume_token(&mut iter, Comma).is_none() {
                errors.add(ErrorKind::MissingComma, current.span.end);
            }

            continue;
        } else if let Some(alias) = parse_use_alias(&mut iter, errors) {
            errors.add(ErrorKind::MissingMultiUseName, alias.span.start);
        }
    }

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::marking_iterator::marking;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokens, test_tokentree};

    #[test]
    fn parse_use_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier, Identifier),
                }),
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
        let mut iter = marking(input.iter());
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier, Identifier),
                }),
                alias: Some(test_token!(Identifier)),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier),
                }),
                alias: None,
                multi: Some(vec![MultiUseNode {
                    span: Span::empty(),
                    name: test_token!(Identifier),
                    alias: None,
                }]),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_multiuse_two_without_alias() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, {Identifier, Comma, Identifier}, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier),
                }),
                alias: None,
                multi: Some(vec![
                    MultiUseNode {
                        span: Span::empty(),
                        name: test_token!(Identifier),
                        alias: None,
                    },
                    MultiUseNode {
                        span: Span::empty(),
                        name: test_token!(Identifier),
                        alias: None,
                    }
                ]),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_multiuse_two_with_alias() {
        // arrange
        let input = test_tokentree!(Use, Identifier, PathSeparator, {Identifier, As, Identifier, Comma, Identifier, As, Identifier}, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier),
                }),
                alias: None,
                multi: Some(vec![
                    MultiUseNode {
                        span: Span::empty(),
                        name: test_token!(Identifier),
                        alias: Some(test_token!(Identifier)),
                    },
                    MultiUseNode {
                        span: Span::empty(),
                        name: test_token!(Identifier),
                        alias: Some(test_token!(Identifier)),
                    }
                ]),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_everything() {
        // arrange
        let input = test_tokentree!(Use:10..13);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (10..13).into(),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (100..103).into(),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (100..110).into(),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (10..30).into(),
                path: Some(PathNode {
                    span: (14..30).into(),
                    parts: test_tokens!(Identifier:14..20, Identifier:22..30),
                    is_rooted: false,
                }),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (3..30).into(),
                path: Some(PathNode {
                    span: (7..20).into(),
                    parts: test_tokens!(Identifier:7..10, Identifier:13..20),
                    is_rooted: false,
                }),
                alias: Some(test_token!(Identifier:23..30)),
                multi: None,
            })
        );
        assert!(errors.has_error_at(30, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_semicolon_after_multi() {
        // arrange
        let input = test_tokentree!(Use:1..4, Identifier:5..10, PathSeparator:10..12, {:14, Identifier:16..20}:22);

        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (1..23).into(),
                path: Some(PathNode {
                    span: (5..10).into(),
                    parts: test_tokens!(Identifier:5..10),
                    is_rooted: false,
                }),
                alias: None,
                multi: Some(vec![MultiUseNode {
                    span: (16..20).into(),
                    name: test_token!(Identifier:16..20),
                    alias: None,
                }]),
            })
        );
        assert!(errors.has_error_at(23, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_alias_name() {
        // arrange
        let input = test_tokentree!(Use:5..8, Identifier:10..13, PathSeparator:13..15, Identifier:15..20, As:21..23, Semicolon:24);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (5..23).into(),
                path: Some(PathNode {
                    span: (10..20).into(),
                    parts: test_tokens!(Identifier:10..13, Identifier:15..20),
                    is_rooted: false,
                }),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (0..25).into(),
                path: Some(PathNode {
                    span: (4..20).into(),
                    parts: test_tokens!(Identifier:4..10, Identifier:13..20),
                    is_rooted: false,
                }),
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
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (0..41).into(),
                path: Some(PathNode {
                    span: (4..10).into(),
                    parts: test_tokens!(Identifier:4..10),
                    is_rooted: false,
                }),
                alias: None,
                multi: Some(vec![
                    MultiUseNode {
                        span: (14..30).into(),
                        name: test_token!(Identifier:14..20),
                        alias: Some(test_token!(Identifier:26..30)),
                    },
                    MultiUseNode {
                        span: (31..40).into(),
                        name: test_token!(Identifier:31..40),
                        alias: None,
                    }
                ]),
            })
        );

        assert!(errors.has_error_at(30, ErrorKind::MissingComma));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn parse_use_missing_name_in_multi() {
        // arrange
        let input = test_tokentree!(Use:0..3, Identifier:4..10, PathSeparator:10..12, {:13, As:23..25, Identifier:26..30}:40, Semicolon:41);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: (0..41).into(),
                path: Some(PathNode {
                    span: (4..10).into(),
                    parts: test_tokens!(Identifier:4..10),
                    is_rooted: false,
                }),
                alias: None,
                multi: Some(vec![]),
            })
        );

        assert!(errors.has_error_at(23, ErrorKind::MissingMultiUseName));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }
}
