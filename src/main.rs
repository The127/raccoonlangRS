use crate::ir::function::generate_function_ir;
use crate::ir::graph::{generate_dot, generate_function_graph};
use crate::modularizer::ModuleRegistry;
use std::fs;
use ustr::ustr;

mod ast;
mod awesome_iterator;
mod codegen;
mod errors;
mod ir;
mod modularizer;
mod parser;
mod scope;
mod source_map;
mod tokenizer;
mod treeizer;

fn main() {
    let mut sources = source_map::SourceCollection::new();
    let mut errors = errors::Errors::new();
    let mut module_registry = ModuleRegistry::new();

    // let input = r#"
    // mod foo;
    // fn qux (a: i32, b: i32) -> i32 {
    //     let a = (10, 12);
    //     10 + {
    //         2 - 1
    //     }
    // }"#;

    let input = r#"mod foo;
fn qux (a: i32, b: i32) -> i32 {
    let n = 1234u32;
    let r = 12.345;
    let s;
    let (x, y) = (a + b, a - b);
    let _ = 1 + 2;
    let (_, (v1, v2), z) = if x > y + 1 {
        (123, (x * y, x / y), 456)
    } else {
        (123, (x / y, x * y), 456)
    };
    v1 - v2 + z
}
    "#;

    let span = sources.load_content(input);
    let tokenizer = tokenizer::tokenize(span, &sources);
    let tt = treeizer::treeize(tokenizer);
    let mut iter = awesome_iterator::make_awesome(tt.iter());
    let file_node = parser::file_node::parse_file(&mut iter, &mut errors);
    let file = ast::file::transform_file(&file_node, &mut errors, &sources);
    for mod_part in file {
        module_registry.register(mod_part);
    }

    module_registry.typecheck(&mut errors);

    dbg!(errors.get_errors());

    for error in errors.get_errors() {
        let printable_fancy = error.printable(&sources);
        printable_fancy.print(std::io::stderr()).unwrap();
    }

    if !errors.get_errors().is_empty() {
        return;
    }

    let mod_foo = module_registry.find(&vec![ustr("foo")]).unwrap();
    let func_qux = mod_foo.get_function(ustr("qux"));
    // dbg!(func_qux);

    let mut package = crate::ir::package::Package::new();
    let mut package_ir = crate::ir::package_ir_builder::PackageIrBuilder::new(&mut package);
    let func_id = package_ir.create_function();
    let mut func_ir = package_ir.function_builder(func_id);
    generate_function_ir(&mut func_ir, func_qux.unwrap());
    let func = package.get_function(func_id);
    println!("{}", func);

    let graph = generate_function_graph(func);
    let graph_dot = generate_dot(func, &graph);

    fs::write("cfg.dot", graph_dot.as_str()).unwrap();

    // dbg!(module_registry);
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        println!("hello!")
    }
}
