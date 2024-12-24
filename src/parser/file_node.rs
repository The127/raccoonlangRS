use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::mod_node::{parse_mod, ModNode};
use crate::parser::recover_until;
use crate::parser::use_node::{parse_use, UseNode};
use crate::source_map::Span;
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct FileNode {
    decls: Vec<TopLevelDeclaration>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevelDeclaration {
    Use(UseNode),
    Mod(ModNode),
}

// TODO: this wants to be tested uwu
pub fn toplevel_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn MarkingIterator<I>,
) -> bool {
    let mut mark = iter.mark().auto_reset();

    let result = match mark.next() {
        Some(TokenTree::Token(Token {
            token_type: Mod | Use,
            ..
        })) => true,
        _ => false,
    };

    result
}

/// A file starts with uses followed by module declarations.
/// However, to support better compiler errors we parse any top level declaration (a declaration that is within the root token tree) here.
pub fn parse_file<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> FileNode {
    let mut result = FileNode {
        decls: vec![],
    };

    while recover_until(iter, errors, [toplevel_starter], []) {
        if let Some(use_node) = parse_use(iter, errors) {
            result.decls.push(TopLevelDeclaration::Use(use_node));
        } else if let Some(mod_node) = parse_mod(iter, errors) {
            result.decls.push(TopLevelDeclaration::Mod(mod_node));
        } else {
            break;
        };
    }

    result
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::marking_iterator::marking;
    use crate::parser::path_node::PathNode;
    use crate::parser::use_node::MultiUseNode;
    use crate::{test_token, test_tokens, test_tokentree};
    use crate::errors::ErrorKind;

    #[test]
    fn parse_file_empty() {
        // arrange
        let input = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![]
            }
        );
    }

    #[test]
    fn parse_file_single_use() {
        let input = test_tokentree!(Use, Identifier, PathSeparator, Identifier, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![TopLevelDeclaration::Use(UseNode {
                    span: Span::empty(),
                    path: Some(PathNode {
                        span: Span::empty(),
                        parts: test_tokens!(Identifier, Identifier),
                        is_rooted: false,
                    }),
                    alias: None,
                    multi: None,
                })]
            }
        );
    }

    #[test]
    fn parse_file_multiple_uses() {
        let input = test_tokentree!(
            Use, Identifier, PathSeparator, Identifier, Semicolon,
            Use, Identifier, PathSeparator, {Identifier, Comma, Identifier}, Semicolon,
        );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![
                    TopLevelDeclaration::Use(UseNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier, Identifier),
                            is_rooted: false,
                        }),
                        alias: None,
                        multi: None,
                    }),
                    TopLevelDeclaration::Use(UseNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier),
                            is_rooted: false,
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
                    }),
                ]
            }
        );
    }

    #[test]
    fn parse_file_single_mod() {
        let input = test_tokentree!(Mod, Identifier, PathSeparator, Identifier, Semicolon);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![TopLevelDeclaration::Mod(ModNode {
                    span: Span::empty(),
                    path: Some(PathNode {
                        span: Span::empty(),
                        parts: test_tokens!(Identifier, Identifier),
                        is_rooted: false,
                    }),
                })]
            }
        );
    }

    #[test]
    fn parse_file_multiple_mods() {
        let input = test_tokentree!(
            Mod,
            Identifier,
            PathSeparator,
            Identifier,
            Semicolon,
            Mod,
            Identifier,
            Semicolon,
        );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![
                    TopLevelDeclaration::Mod(ModNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier, Identifier),
                            is_rooted: false,
                        }),
                    }),
                    TopLevelDeclaration::Mod(ModNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier),
                            is_rooted: false,
                        }),
                    }),
                ]
            }
        );
    }

    #[test]
    fn parse_file_interspersed_mods_and_uses() {
        let input = test_tokentree!(
            Mod, Identifier, PathSeparator, Identifier, Semicolon,
            Use, Identifier, Semicolon,
            Mod, Identifier, Semicolon,
            Use, PathSeparator, Identifier, Semicolon,
        );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(
            file,
            FileNode {
                decls: vec![
                    TopLevelDeclaration::Mod(ModNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier, Identifier),
                            is_rooted: false,
                        }),
                    }),
                    TopLevelDeclaration::Use(UseNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier),
                            is_rooted: false,
                        }),
                        alias: None,
                        multi: None,
                    }),
                    TopLevelDeclaration::Mod(ModNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier),
                            is_rooted: false,
                        }),
                    }),
                    TopLevelDeclaration::Use(UseNode {
                        span: Span::empty(),
                        path: Some(PathNode {
                            span: Span::empty(),
                            parts: test_tokens!(Identifier),
                            is_rooted: true,
                        }),
                        alias: None,
                        multi: None,
                    }),
                ]
            }
        );
    }

    #[test]
    fn parse_file_unexpected_tokens() {
        let input = test_tokentree!(
            Unknown:1..3,
            Mod:5..8, Identifier:9..14, Semicolon:14,
            Unknown:15..20,
            Use:23..26, Identifier:27..30, Semicolon:30,
            Unknown:35..40,
        );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(file, FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(ModNode {
                    span: (5..14).into(),
                    path: Some(PathNode {
                        span: (9..14).into(),
                        parts: test_tokens!(Identifier:9..14),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Use(UseNode {
                    span: (23..30).into(),
                    path: Some(PathNode {
                        span: (27..30).into(),
                        parts: test_tokens!(Identifier:27..30),
                        is_rooted: false,
                    }),
                    alias: None,
                    multi: None,
                })
            ],
        });

        assert!(errors.has_error_at(1..3, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(15..20, ErrorKind::UnexpectedToken(Unknown)));
        assert!(errors.has_error_at(35..40, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 3);
    }

    #[test]
    fn parse_file_missing_semicolon() {
        let input = test_tokentree!(
            Mod:5..8, Identifier:9..14,
            Use:23..26, Identifier:27..30, Semicolon:30,
        );
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let file = parse_file(&mut iter, &mut errors);

        // assert
        assert_eq!(file, FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(ModNode {
                    span: (5..14).into(),
                    path: Some(PathNode {
                        span: (9..14).into(),
                        parts: test_tokens!(Identifier:9..14),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Use(UseNode {
                    span: (23..30).into(),
                    path: Some(PathNode {
                        span: (27..30).into(),
                        parts: test_tokens!(Identifier:27..30),
                        is_rooted: false,
                    }),
                    alias: None,
                    multi: None,
                })
            ],
        });

        assert!(errors.has_error_at(14, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
    }
}
