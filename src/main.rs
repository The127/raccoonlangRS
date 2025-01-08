use crate::modularizer::ModuleRegistry;
use ustr::ustr;

mod ast;
mod codegen;
mod errors;
mod marking_iterator;
mod modularizer;
mod parser;
mod source_map;
mod tokenizer;
mod treeizer;
mod until_iterator;

fn main() {
    let mut sources = source_map::SourceCollection::new();
    let mut errors = errors::Errors::new();
    let mut module_registry = ModuleRegistry::new();

    let input = "use foo::bar; mod foo; fn qux (a: int, b: int) -> int { 10 + { 2 - 1 } }";
    let span = sources.load_content(input);
    let tokenizer = tokenizer::tokenize(span, &sources);
    let tt = treeizer::treeize(tokenizer);
    let mut iter = marking_iterator::marking(tt.iter());
    let file_node = parser::file_node::parse_file(&mut iter, &mut errors);
    let file = ast::file::transform_file(&file_node, &sources);
    for mod_part in file {
        module_registry.register(mod_part);
    }

    dbg!(module_registry);
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        println!("hello!")
    }
}
