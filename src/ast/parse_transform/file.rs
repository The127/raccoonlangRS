use crate::ast::mod_part::ModPart;
use crate::ast::parse_transform::function::transform_function_decl;
use crate::ast::parse_transform::struct_::transform_struct_decl;
use crate::errors::Errors;
use crate::parser::file_node::{FileNode, TopLevelDeclaration};
use crate::source_map::{HasSpan, SourceCollection, Span};

pub fn transform_file(
    node: &FileNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Vec<Box<ModPart>> {
    let mut mod_parts = vec![];

    let mut current_part = Box::new(ModPart::new(Span::empty(), vec![]));

    let mut current_is_first = true;

    for decl in &node.decls {
        match decl {
            TopLevelDeclaration::Mod(module) => {
                if !current_is_first || !current_part.is_empty() {
                    mod_parts.push(current_part);
                }

                current_is_first = false;

                current_part = Box::new(ModPart::new(
                    module.span(),
                    module
                        .path
                        .as_ref()
                        .map(|p| {
                            p.parts
                                .iter()
                                .map(|t| sources.get_identifier(t.span()))
                                .collect()
                        })
                        .unwrap_or(vec![]),
                ));
            }
            TopLevelDeclaration::Fn(fn_node) => {
                let f = transform_function_decl(fn_node, errors, sources);
                current_part.set_span(current_part.span() + f.span());
                current_part.functions.push(f);
            }
            TopLevelDeclaration::Struct(struct_node) => {
                let s = transform_struct_decl(struct_node, errors, sources);
                current_part.set_span(current_part.span() + s.span());
                current_part.structs.push(s);
            }
            TopLevelDeclaration::Use(_) => {
                todo!()
            }
        }
    }
    if !current_is_first || !current_part.is_empty() {
        mod_parts.push(current_part);
    }

    mod_parts
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::errors::Errors;
    use crate::parser::file_node::{FileNode, TopLevelDeclaration};
    use crate::parser::fn_node::parse_fn;
    use crate::parser::mod_node::parse_mod;
    use crate::parser::struct_node::parse_struct;
    use crate::parser::test_utils::test_parse_from_string;
    use crate::source_map::{HasSpan, SourceCollection};
    use ustr::{ustr, Ustr};

    #[test]
    fn transform_empty_file() {
        // arrange
        let sources = SourceCollection::new();
        let mut errors = Errors::new();
        let file = FileNode { decls: vec![] };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert
        assert!(mod_parts.is_empty());
    }

    #[test]
    fn transform_fn_before_mod() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();

        let fn_node = test_parse_from_string(&mut sources, "fn foobar() {}", |iter, errors| {
            parse_fn(iter, errors)
        });

        let file = FileNode {
            decls: vec![TopLevelDeclaration::Fn(fn_node)],
        };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert

        assert_eq!(mod_parts.len(), 1);
        assert_eq!(mod_parts[0].path, Vec::<Ustr>::new());
        assert_eq!(mod_parts[0].functions.len(), 1);
    }

    #[test]
    fn transform_single_empty_module_part() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();

        let mod_node = test_parse_from_string(&mut sources, "mod foo::bar", |iter, errors| {
            parse_mod(iter, errors)
        });
        let span = mod_node.span();

        let file = FileNode {
            decls: vec![TopLevelDeclaration::Mod(mod_node)],
        };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![Box::new(ModPart::new(span, vec![ustr("foo"), ustr("bar")]))]
        );
    }

    #[test]
    fn transform_multiple_empty_module_parts_same_name() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();

        let mod_node_1 = test_parse_from_string(&mut sources, "mod foo::bar", |iter, errors| {
            parse_mod(iter, errors)
        });
        let mod_node_2 = test_parse_from_string(&mut sources, "mod foo::bar", |iter, errors| {
            parse_mod(iter, errors)
        });
        let span_1 = mod_node_1.span();
        let span_2 = mod_node_2.span();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(mod_node_1),
                TopLevelDeclaration::Mod(mod_node_2),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![
                Box::new(ModPart::new(span_1, vec![ustr("foo"), ustr("bar")])),
                Box::new(ModPart::new(span_2, vec![ustr("foo"), ustr("bar")]))
            ]
        );
    }

    #[test]
    fn transform_multiple_empty_module_parts_different_names() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();

        let mod_node_1 = test_parse_from_string(&mut sources, "mod foo::bar", |iter, errors| {
            parse_mod(iter, errors)
        });
        let mod_node_2 = test_parse_from_string(&mut sources, "mod qux", |iter, errors| {
            parse_mod(iter, errors)
        });

        let span_1 = mod_node_1.span();
        let span_2 = mod_node_2.span();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(mod_node_1),
                TopLevelDeclaration::Mod(mod_node_2),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert
        assert_eq!(
            mod_parts,
            vec![
                Box::new(ModPart::new(span_1, vec![ustr("foo"), ustr("bar")])),
                Box::new(ModPart::new(span_2, vec![ustr("qux")]))
            ]
        );
    }

    #[test]
    fn transform_fn_in_first_mod() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();

        let mod_node_1 = test_parse_from_string(&mut sources, "mod foo::bar", |iter, errors| {
            parse_mod(iter, errors)
        });
        let fn_node = test_parse_from_string(&mut sources, "fn foobar() {}", |iter, errors| {
            parse_fn(iter, errors)
        });
        let mod_node_2 = test_parse_from_string(&mut sources, "mod qux", |iter, errors| {
            parse_mod(iter, errors)
        });

        let span_mod1 = mod_node_1.span() + fn_node.span();
        let span_mod2 = mod_node_2.span();
        let span_fn = fn_node.span();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(mod_node_1),
                TopLevelDeclaration::Fn(fn_node),
                TopLevelDeclaration::Mod(mod_node_2),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert

        assert_eq!(mod_parts.len(), 2);
        assert_eq!(mod_parts[0].span(), span_mod1);
        assert_eq!(mod_parts[0].path, vec![ustr("foo"), ustr("bar")]);

        assert_eq!(mod_parts[0].functions.len(), 1);
        assert_eq!(mod_parts[0].functions[0].span(), span_fn);

        assert_eq!(mod_parts[1].span(), span_mod2);
        assert_eq!(mod_parts[1].path, vec![ustr("qux")]);
        assert!(mod_parts[1].functions.is_empty());
    }

    #[test]
    fn transform_struct_in_first_mod() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();

        let mod_node_1 = test_parse_from_string(&mut sources, "mod foo::bar", |iter, errors| {
            parse_mod(iter, errors)
        });
        let struct_node =
            test_parse_from_string(&mut sources, "struct FooBar()", |iter, errors| {
                parse_struct(iter, errors)
            });
        let mod_node_2 = test_parse_from_string(&mut sources, "mod qux", |iter, errors| {
            parse_mod(iter, errors)
        });

        let span_mod1 = mod_node_1.span() + struct_node.span();
        let span_mod2 = mod_node_2.span();
        let span_struct = struct_node.span();

        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(mod_node_1),
                TopLevelDeclaration::Struct(struct_node),
                TopLevelDeclaration::Mod(mod_node_2),
            ],
        };

        // act
        let mod_parts = transform_file(&file, &mut errors, &sources);

        // assert

        assert_eq!(mod_parts.len(), 2);
        assert_eq!(mod_parts[0].span(), span_mod1);
        assert_eq!(mod_parts[0].path, vec![ustr("foo"), ustr("bar")]);

        assert_eq!(mod_parts[0].structs.len(), 1);
        assert_eq!(mod_parts[0].structs[0].span(), span_struct);

        assert_eq!(mod_parts[1].span(), span_mod2);
        assert_eq!(mod_parts[1].path, vec![ustr("qux")]);
        assert!(mod_parts[1].functions.is_empty());
    }
}
