use crate::modularizer::ModuleRegistry;

mod ast;
mod codegen;
mod errors;
mod awesome_iterator;
mod modularizer;
mod parser;
mod source_map;
mod tokenizer;
mod treeizer;
mod ir;
mod scope;

fn main() {
    let mut sources = source_map::SourceCollection::new();
    let mut errors = errors::Errors::new();
    let mut module_registry = ModuleRegistry::new();

    let input = r#"
    mod foo;
    fn qux (a: i32, b: i32) -> i32 {
        let a = (10, 12);
        10 + {
            2 - 1
        }
    }"#;
    let span = sources.load_content(input);
    let tokenizer = tokenizer::tokenize(span, &sources);
    let tt = treeizer::treeize(tokenizer);
    let mut iter = awesome_iterator::make_awesome(tt.iter());
    let file_node = parser::file_node::parse_file(&mut iter, &mut errors);
    let file = ast::file::transform_file(&file_node, &sources);
    for mod_part in file {
        module_registry.register(mod_part);
    }

    module_registry.typecheck(&mut errors);

    dbg!(module_registry);

    dbg!(errors.get_errors());
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        println!("hello!")
    }
}
