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
        [path_starter, alias_starter, multi_use_starter],
        [toplevel_starter],
    ) {
        // TODO: register error: missing use path
        return Some(result);
    }

    if let Some(path) = parse_path(iter) {
        result.span += path.span;
        result.path = Some(path);
    } else {
        // TODO: register error: missing use path
    }

    if !recover_until(iter, [alias_starter, multi_use_starter], [toplevel_starter]) {
        // TODO: register error: missing semicolon
        return Some(result);
    }

    if let Some(alias) = parse_use_alias(iter) {
        result.span += alias.span;
        result.alias = alias.name;
    } else if let Some(multi) = parse_multi_use(iter) {
        result.span += multi.span;
        result.multi = Some(multi.value);
    }

    // //TODO: only allow this if no alias
    // // we don't need to recover here since it is optional
    // if let Some(group) = consume_group(&mut iter, OpenCurly) {
    //     // TODO: handle broken group error (maybe in the consume group function)
    //     //TODO: handle content of group
    // }

    Some(result)
}

#[derive(Default)]
struct Alias {
    span: Span,
    name: Option<Token>,
}

token_starter!(alias_starter, As);
fn parse_use_alias<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
) -> Option<Alias> {
    let mut result = Alias::default();

    if let Some(as_token) = consume_token(iter, As) {
        result.span = as_token.span;
    } else {
        return None;
    }

    token_starter!(identifier, Identifier);
    if !recover_until(iter, [identifier], [toplevel_starter]) {
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

tt_starter!(multi_use_starter, OpenCurly);
fn parse_multi_use<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
) -> Option<Spanned<Vec<MultiUseNode>>> {
    let group = match consume_group(iter, OpenCurly) {
        Some(group) => group,
        _ => return None,
    };

    let mut iter = marking(group.children.iter());

    let mut result: Spanned<Vec<MultiUseNode>> = Spanned {
        span: group.span(),
        ..Spanned::default()
    };

    loop {
        token_starter!(identifier, Identifier);
        token_starter!(comma, Comma);
        recover_until(&mut iter, [identifier, comma], []); // maybe?
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
            if !recover_until(&mut iter, [alias_starter, comma], [identifier]) {
                // TODO: error missing comma
                continue;
            }
            if let Some(alias) = parse_use_alias(&mut iter) {
                current.span += alias.span;
                current.alias = alias.name;
            }

            if !recover_until(&mut iter, [comma], [identifier]) {
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
    use crate::{test_token, test_tokens, test_tokentree, test_tokentree_helper};

    #[test]
    fn parse_use_empty() {
        // arrange
        let input: Vec<TokenTree> = vec![];

        // act
        let result = parse_use(&mut marking(input.iter()));

        // assert
        assert_eq!(result, None)
    }

    #[test]
    fn parse_use_one_use() {
        // arrange
        let input = test_tokentree!(Use Identifier PathSeparator Identifier Semicolon);

        // act
        let result = parse_use(&mut marking(input.iter()));

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier Identifier),
                }),
                alias: None,
                multi: None,
            })
        );
    }

    #[test]
    fn parse_use_one_use_with_alias() {
        // arrange
        let input =
            test_tokentree!(Use Identifier PathSeparator Identifier As Identifier Semicolon);

        // act
        let result = parse_use(&mut marking(input.iter()));

        // assert
        assert_eq!(
            result,
            Some(UseNode {
                span: Span::empty(),
                path: Some(PathNode {
                    span: Span::empty(),
                    is_rooted: false,
                    parts: test_tokens!(Identifier Identifier),
                }),
                alias: Some(test_token!(Identifier)),
                multi: None,
            })
        );
    }

    #[test]
    fn parse_use_multiuse_one_without_alias() {
        // arrange
        let input = test_tokentree!(Use Identifier PathSeparator {Identifier} Semicolon);

        // act
        let result = parse_use(&mut marking(input.iter()));

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
    }

    #[test]
    fn parse_use_multiuse_two_without_alias() {
        // arrange
        let input =
            test_tokentree!(Use Identifier PathSeparator {Identifier Comma Identifier} Semicolon);

        // act
        let result = parse_use(&mut marking(input.iter()));

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
    }

    #[test]
    fn parse_use_multiuse_two_with_alias() {
        // arrange
        let input =
            test_tokentree!(Use Identifier PathSeparator {Identifier As Identifier Comma Identifier As Identifier} Semicolon);

         // act
        let result = parse_use(&mut marking(input.iter()));

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
    }
}
