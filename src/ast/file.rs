use crate::ast::function_decl::{transform_function_decl, FunctionDecl};
use crate::parser::file_node::{FileNode, TopLevelDeclaration};
use crate::source_map::{SourceCollection, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq)]
pub struct ModPart {
    pub span: Span,
    pub path: Vec<Ustr>,
    // pub uses: Uses, //TODO:
    pub functions: Vec<FunctionDecl>,
}

impl ModPart {
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

pub struct Uses {}

pub fn transform_file(node: &FileNode, sources: &SourceCollection) -> Vec<ModPart> {
    let mut mod_parts = vec![];

    let mut current_part = ModPart {
        span: Span::empty(),
        path: vec![],
        functions: vec![],
    };

    let mut current_is_first = true;

    for decl in &node.decls {
        match decl {
            TopLevelDeclaration::Mod(module) => {
                if !current_is_first || !current_part.is_empty() {
                    mod_parts.push(current_part);
                }

                current_is_first = false;

                current_part = ModPart {
                    span: module.span,
                    path: module
                        .path
                        .as_ref()
                        .map(|p| {
                            p.parts
                                .iter()
                                .map(|t| sources.get_identifier(t.span))
                                .collect()
                        })
                        .unwrap_or(vec![]),
                    functions: vec![],
                };
            }
            TopLevelDeclaration::Fn(fn_node) => {
                let f = transform_function_decl(fn_node, sources);
                current_part.span += f.span;
                current_part.functions.push(f);
            }
            TopLevelDeclaration::Use(use_node) => {
                //TODO:
            }
            _ => unreachable!(),
        }
    }
    if !current_is_first || !current_part.is_empty() {
        mod_parts.push(current_part);
    }

    mod_parts
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::file::{transform_file, ModPart};
    use crate::ast::function_decl::{FunctionDecl, FunctionReturnType};
    use crate::ast::types::Type;
    use crate::ast::Visibility;
    use crate::parser::file_node::{FileNode, TopLevelDeclaration};
    use crate::parser::fn_node::FnNode;
    use crate::parser::mod_node::ModNode;
    use crate::parser::path_node::PathNode;
    use crate::parser::Visibility as ParserVisibility;
    use crate::source_map::{SourceCollection, Span};
    use crate::test_tokens;
    use crate::tokenizer::TokenType::Identifier;
    use ustr::ustr;

    #[test]
    fn transform_empty_file() {
        // arrange
        let sources = SourceCollection::new();
        let file = FileNode { decls: vec![] };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert!(mod_parts.is_empty());
    }

    #[test]
    fn transform_fn_before_mod() {
        // arrange
        let mut sources = SourceCollection::new();

        let span = sources.load_content("mod foo::bar");
        let path_span: Span = ((span.start + 4)..span.end).into();
        let foo_span: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span: Span = ((path_span.end - 3)..path_span.end).into();

        let file = FileNode {
            decls: vec![TopLevelDeclaration::Fn(FnNode {
                span,
                visibility: ParserVisibility::Module,
                name: None,
                parameters: vec![],
                return_type: None,
                body: None,
            })],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![ModPart {
                span: span,
                path: vec![],
                functions: vec![FunctionDecl {
                    span,
                    name: None,
                    visibility: Visibility::Module,
                    parameters: vec![],
                    return_type: FunctionReturnType { type_: Type::Unit },
                    body: Expression::unknown(),
                }],
            }]
        );
    }

    #[test]
    fn transform_single_empty_module_part() {
        // arrange
        let mut sources = SourceCollection::new();

        let span = sources.load_content("mod foo::bar");
        let path_span: Span = ((span.start + 4)..span.end).into();
        let foo_span: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span: Span = ((path_span.end - 3)..path_span.end).into();

        let file = FileNode {
            decls: vec![TopLevelDeclaration::Mod(ModNode {
                span: span,
                path: Some(PathNode {
                    span: path_span,
                    parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                    is_rooted: false,
                }),
            })],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![ModPart {
                span: span,
                path: vec![ustr("foo"), ustr("bar")],
                functions: vec![],
            }]
        );
    }

    #[test]
    fn transform_multiple_empty_module_parts_same_name() {
        // arrange
        let mut sources = SourceCollection::new();

        let span = sources.load_content("mod foo::bar");
        let path_span: Span = ((span.start + 4)..span.end).into();
        let foo_span: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span: Span = ((path_span.end - 3)..path_span.end).into();

        let span2 = sources.load_content("mod foo::bar");
        let path_span2: Span = ((span2.start + 4)..span2.end).into();
        let foo_span2: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span2: Span = ((path_span.end - 3)..path_span.end).into();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(ModNode {
                    span: span,
                    path: Some(PathNode {
                        span: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Mod(ModNode {
                    span: span2,
                    path: Some(PathNode {
                        span: path_span2,
                        parts: test_tokens!(Identifier:foo_span2, Identifier:bar_span2),
                        is_rooted: false,
                    }),
                }),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![
                ModPart {
                    span: span,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![],
                },
                ModPart {
                    span: span2,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![],
                }
            ]
        );
    }

    #[test]
    fn transform_multiple_empty_module_parts_different_names() {
        // arrange
        let mut sources = SourceCollection::new();

        let span = sources.load_content("mod foo::bar");
        let path_span: Span = ((span.start + 4)..span.end).into();
        let foo_span: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span: Span = ((path_span.end - 3)..path_span.end).into();

        let span2 = sources.load_content("mod qux");
        let path_span2: Span = ((span2.start + 4)..span2.end).into();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(ModNode {
                    span: span,
                    path: Some(PathNode {
                        span: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Mod(ModNode {
                    span: span2,
                    path: Some(PathNode {
                        span: path_span2,
                        parts: test_tokens!(Identifier:path_span2),
                        is_rooted: false,
                    }),
                }),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![
                ModPart {
                    span: span,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![],
                },
                ModPart {
                    span: span2,
                    path: vec![ustr("qux")],
                    functions: vec![],
                }
            ]
        );
    }

    #[test]
    fn transform_fn_in_first_mod() {
        // arrange
        let mut sources = SourceCollection::new();

        let span = sources.load_content("mod foo::bar");
        let path_span: Span = ((span.start + 4)..span.end).into();
        let foo_span: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span: Span = ((path_span.end - 3)..path_span.end).into();

        let span2 = sources.load_content("mod qux");
        let path_span2: Span = ((span2.start + 4)..span2.end).into();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(ModNode {
                    span: span,
                    path: Some(PathNode {
                        span: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Fn(FnNode {
                    span,
                    visibility: ParserVisibility::Module,
                    name: None,
                    parameters: vec![],
                    return_type: None,
                    body: None,
                }),
                TopLevelDeclaration::Mod(ModNode {
                    span: span2,
                    path: Some(PathNode {
                        span: path_span2,
                        parts: test_tokens!(Identifier:path_span2),
                        is_rooted: false,
                    }),
                }),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![
                ModPart {
                    span: span,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![FunctionDecl {
                        span,
                        name: None,
                        visibility: Visibility::Module,
                        parameters: vec![],
                        return_type: FunctionReturnType { type_: Type::Unit },
                        body: Expression::unknown(),
                    }],
                },
                ModPart {
                    span: span2,
                    path: vec![ustr("qux")],
                    functions: vec![],
                }
            ]
        );
    }
}
