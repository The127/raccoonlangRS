use ustr::ustr;
use crate::ir::function::generate_function_ir;
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
pub mod scope;

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

    let input = r#"
    mod foo;
    fn qux (a: i32, b: i32) -> i32 {
        let x = a + b;
        let y = a - b;
        if x > y + 1 {
            x * y
        } else {
            x / y
        }
    }
    "#;

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

    dbg!(errors.get_errors());

    let mod_foo = module_registry.find(&vec![ustr("foo")]).unwrap();
    let func_qux = mod_foo.get_function(ustr("qux"));
    // dbg!(func_qux);

    let mut package = crate::ir::package::Package::new();
    let mut package_ir = crate::ir::package_ir_builder::PackageIrBuilder::new(&mut package);
    let func_id = generate_function_ir(&mut package_ir, func_qux.unwrap());
    let func = package.get_function(func_id);
    println!("{}", func);

    // dbg!(module_registry);
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        println!("hello!")
    }
}
