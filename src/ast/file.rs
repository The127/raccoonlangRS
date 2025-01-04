use ustr::Ustr;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::uses::Uses;
use crate::parser::file_node::{FileNode, TopLevelDeclaration};
use crate::source_map::{SourceCollection, Span};

#[derive(Debug, Eq, PartialEq)]
pub struct ModPart {
    pub span: Span,
    pub path: Vec<Ustr>,
    // pub uses: Uses,
    // pub functions: Vec<FunctionDecl>,
}

pub fn transform_file(node: &FileNode, sources: &SourceCollection) -> Vec<ModPart> {
    if let Some(TopLevelDeclaration::Mod(mod_node)) = node.decls.first()  {
        return vec![ModPart {
            span: mod_node.span,
            path: mod_node.path.as_ref().map(|p| p.parts.iter().map(|t| sources.get_identifier(t.span)).collect()).unwrap_or(vec![]),
        }]
    }
    vec![]
}

#[cfg(test)]
mod test {
    use ustr::ustr;
    use crate::ast::file::{transform_file, ModPart};
    use crate::ast::uses::Uses;
    use crate::parser::file_node::{FileNode, TopLevelDeclaration};
    use crate::parser::mod_node::ModNode;
    use crate::parser::path_node::PathNode;
    use crate::source_map::{SourceCollection, Span};
    use crate::test_tokens;
    use crate::tokenizer::TokenType::Identifier;

    #[test]
    fn transform_empty_file(){
        // arrange
        let sources = SourceCollection::new();
        let file = FileNode {
            decls: vec![],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert!(mod_parts.is_empty());
    }

    #[test]
    fn transform_single_empty_module_part(){
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("mod foo::bar");
        let path_span: Span = ((span.start + 4)..span.end).into();
        let foo_span: Span = (path_span.start..(path_span.start + 3)).into();
        let bar_span: Span = ((path_span.end - 3)..path_span.end).into();
        let file = FileNode {
            decls: vec![
                TopLevelDeclaration::Mod(ModNode {
                    span: span,
                    path: Some(PathNode {
                        span: path_span,
                        parts: test_tokens!(Identifier:foo_span, Identifier:bar_span),
                        is_rooted: false,
                    }),
                })
            ],
        };

        // act
        let mod_parts = transform_file(&file, &sources);

        // assert
        assert_eq!(mod_parts, vec![
            ModPart {
                span: span,
                path: vec![ustr("foo"), ustr("bar")]
            }
        ]);
    }
}