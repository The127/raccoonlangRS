use crate::errors::{ErrorKind, Errors};
use crate::marking_iterator::{marking, MarkingIterator};
use crate::parser::file_node::toplevel_starter;
use crate::parser::path_node::{parse_path, path_starter, PathNode};
use crate::parser::*;
use crate::source_map::Span;
use crate::tokenizer::TokenType::*;
use crate::treeizer::*;
use crate::{token_starter, tt_starter};
use std::ops::Add;

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
        [path_starter, alias_starter, multi_use_starter],
        [toplevel_starter],
    ) {
        errors.add(ErrorKind::MissingUsePath, result.span.end.into());
        return Some(result);
    }

    if let Some(path) = parse_path(iter, errors) {
        result.span += path.span;
        result.path = Some(path);
    } else {
        // TODO: register error: missing use path
    }

    if !recover_until(
        iter,
        errors,
        [alias_starter, multi_use_starter, semicolon],
        [toplevel_starter],
    ) {
        // TODO: register error: missing semicolon
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
        // TODO: register error: missing semicolon
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
    if !recover_until(iter, errors, [identifier], [toplevel_starter]) {
        // TODO: register error: missing alias name thingie
        return Some(result);
    }

    result.name = Some(expect_token(iter, Identifier));

    Some(result)
}

#[derive(Debug, Eq, PartialEq)]
pub struct MultiUseNode {
    pub span: Span,
    pub name: Token,
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

    loop {
        token_starter!(identifier, Identifier);
        token_starter!(comma, Comma);
        recover_until(&mut iter, errors, [identifier, comma], []); // maybe?
        if let Some(name) = consume_token(&mut iter, Identifier) {
            result.value.push(MultiUseNode {
                name: name,
                span: name.span,
                alias: None,
            });
            let mut current = result
                .value
                .last_mut()
                .expect("literally just pushed a value...");
            if !recover_until(&mut iter, errors, [alias_starter, comma], [identifier]) {
                // TODO: error missing comma
                continue;
            }
            if let Some(alias) = parse_use_alias(&mut iter, errors) {
                current.span += alias.span;
                current.alias = alias.name;
            }

            if !recover_until(&mut iter, errors, [comma], [identifier]) {
                // TODO: error missing comma
                continue;
            }
            expect_token(&mut iter, Comma);
            continue;
        } else if let Some(comma_token) = consume_token(&mut iter, Comma) {
            // TODO: error missing thingie
        } else {
            break;
        }
    }

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::marking_iterator::marking;
    use crate::tokenizer::Token;
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
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(
            iter.collect::<Vec<_>>(),
            test_tokentree!(Mod).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_use_one_use_with_alias() {
        // arrange
        let input =
            test_tokentree!(Use, Identifier, PathSeparator, Identifier, As, Identifier, Semicolon);
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
        let input = test_tokentree!(Use, Identifier, PathSeparator, {Identifier}, Semicolon);
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
        let input =
            test_tokentree!(Use, Identifier, PathSeparator, {Identifier, Comma, Identifier}, Semicolon);
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
        let input = test_tokentree!(Use);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_use(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(UseNode {
            span: Span::empty(),
            path: None,
            alias: None,
            multi: None,
        }));
        assert!(errors.get_errors().iter().find(|e| e.kind == ErrorKind::MissingUsePath).is_some());
        assert!(errors.get_errors().iter().find(|e| e.kind == ErrorKind::MissingSemicolon).is_some());
        assert_eq!(errors.get_errors().len(), 2);
        assert!(iter.collect::<Vec<_>>().is_empty());
    }
}
