use crate::ast::function_decl::{transform_function_decl, FunctionDecl};
use crate::parser::file_node::{FileNode, TopLevelDeclaration};
use crate::source_map::{HasSpan, SourceCollection, Span};
use ustr::Ustr;
use crate::tokenizer::Token;

#[derive(Debug, Eq, PartialEq)]
pub struct ModPart {
    span_: Span,
    pub path: Vec<Ustr>,
    // pub uses: Uses, //TODO:
    pub functions: Vec<FunctionDecl>,
}

impl ModPart {
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

impl HasSpan for ModPart {
    fn span(&self) -> Span {
        self.span_
    }
}

pub struct Uses {}

pub fn transform_file(node: &FileNode, sources: &SourceCollection) -> Vec<Box<ModPart>> {
    let mut mod_parts = vec![];

    let mut current_part = Box::new(ModPart {
        span_: Span::empty(),
        path: vec![],
        functions: vec![],
    });

    let mut current_is_first = true;

    for decl in &node.decls {
        match decl {
            TopLevelDeclaration::Mod(module) => {
                if !current_is_first || !current_part.is_empty() {
                    mod_parts.push(current_part);
                }

                current_is_first = false;

                current_part = Box::new(ModPart {
                    span_: module.span(),
                    path: module
                        .path
                        .as_ref()
                        .map(|p| {
                            p.parts
                                .iter()
                                .map(|t| sources.get_identifier(t.span()))
                                .collect()
                        })
                        .unwrap_or(vec![]),
                    functions: vec![],
                });
            }
            TopLevelDeclaration::Fn(fn_node) => {
                let f = transform_function_decl(fn_node, sources);
                current_part.span_ += f.span();
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
    use std::slice::Iter;
    use crate::ast::expressions::Expression;
    use crate::ast::file::{transform_file, ModPart};
    use crate::ast::function_decl::{FunctionDecl, FunctionReturnType};
    use crate::ast::types::Type;
    use crate::ast::Visibility;
    use crate::parser::file_node::{FileNode, TopLevelDeclaration};
    use crate::parser::fn_node::{parse_fn, FnNode};
    use crate::parser::mod_node::ModNode;
    use crate::parser::path_node::PathNode;
    use crate::parser::Visibility as ParserVisibility;
    use crate::source_map::{HasSpan, SourceCollection, Span};
    use crate::{marking_iterator, test_tokens, tokenizer, treeizer};
    use crate::tokenizer::TokenType::Identifier;
    use ustr::{ustr, Ustr};
    use crate::errors::Errors;
    use crate::marking_iterator::{marking, MarkingIterator};
    use crate::tokenizer::tokenize;
    use crate::treeizer::{treeize, TokenTree};

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

    fn make_test_tokentree_from(sources: &mut SourceCollection, input: &str) -> Vec<TokenTree> {
        let span = sources.load_content(input);
        let tokenizer = tokenize(span, &sources);
        let tt = treeize(tokenizer);
        return tt;
    }

    fn make_thingie<T>(sources: &mut SourceCollection, input: &str,
                       parser: impl std::ops::Fn(&mut dyn MarkingIterator<Iter<TokenTree>>,
                                  &mut Errors,
                       ) -> Option<T>) -> T {
        let mut errors = Errors::new();
        let tt = make_test_tokentree_from(sources, input);
        let mut iter = marking(tt.iter());
        let result = parser(&mut iter, &mut errors).unwrap();
        result
    }

    #[test]
    fn transform_fn_before_mod() {
        // arrange
        let mut sources = SourceCollection::new();

        let fn_node = make_thingie(&mut sources, "mod foo::bar", parse_fn);

        // let fn_node = parse_fn(&mut iter, &mut errors).unwrap();

        let file = FileNode {
            decls: vec![TopLevelDeclaration::Fn(fn_node)],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert

        assert_eq!(mod_parts.len(), 1);
        assert_eq!(mod_parts[0].path, Vec::<Ustr>::new());
        assert_eq!(mod_parts[0].functions.len(), 1);
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
                span_: span,
                path: Some(PathNode {
                    span_: path_span,
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
            vec![Box::new(ModPart {
                span_: span,
                path: vec![ustr("foo"), ustr("bar")],
                functions: vec![],
            })]
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
                    span_: span,
                    path: Some(PathNode {
                        span_: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Mod(ModNode {
                    span_: span2,
                    path: Some(PathNode {
                        span_: path_span2,
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
                Box::new(ModPart {
                    span_: span,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![],
                }),
                Box::new(ModPart {
                    span_: span2,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![],
                })
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
                    span_: span,
                    path: Some(PathNode {
                        span_: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Mod(ModNode {
                    span_: span2,
                    path: Some(PathNode {
                        span_: path_span2,
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
                Box::new(ModPart {
                    span_: span,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![],
                }),
                Box::new(ModPart {
                    span_: span2,
                    path: vec![ustr("qux")],
                    functions: vec![],
                })
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
                    span_: span,
                    path: Some(PathNode {
                        span_: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                }),
                TopLevelDeclaration::Fn(FnNode {
                    span_: span,
                    visibility: ParserVisibility::Module,
                    name: None,
                    parameters: vec![],
                    return_type: None,
                    body: None,
                }),
                TopLevelDeclaration::Mod(ModNode {
                    span_: span2,
                    path: Some(PathNode {
                        span_: path_span2,
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
                Box::new(ModPart {
                    span_: span,
                    path: vec![ustr("foo"), ustr("bar")],
                    functions: vec![FunctionDecl {
                        span_: span,
                        name: None,
                        visibility: Visibility::Module,
                        parameters: vec![],
                        return_type: FunctionReturnType { type_: Type::Unit },
                        body: Expression::unknown(),
                    }],
                }),
                Box::new(ModPart {
                    span_: span2,
                    path: vec![ustr("qux")],
                    functions: vec![],
                })
            ]
        );
    }
}
