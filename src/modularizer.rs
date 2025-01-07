use crate::ast::file::ModPart;
use std::collections::HashMap;
use ustr::Ustr;

#[derive(Eq, PartialEq, Debug)]
pub struct ModuleRegistry {
    modules: HashMap<Vec<Ustr>, Module>,
}

impl ModuleRegistry {
    pub fn new() -> ModuleRegistry {
        ModuleRegistry {
            modules: Default::default(),
        }
    }

    pub fn register(&mut self, mod_part: Box<ModPart>) {
        if !self.modules.contains_key(&mod_part.path) {
            self.modules.insert(mod_part.path.clone(), Module {
                parts: vec![],
            });
        }

        if let Some(module) = self.modules.get_mut(&mod_part.path){
            module.parts.push(mod_part);
        }
    }

    pub fn find(&self, path: &Vec<Ustr>) -> Option<&Module> {
        self.modules.get(path)
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Module {
    parts: Vec<Box<ModPart>>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::file::ModPart;
    use crate::source_map::Span;
    use ustr::ustr;

    #[test]
    fn find_unknown_module() {
        // arrange
        let mut module_registry = ModuleRegistry::new();

        // act
        let module = module_registry.find(&vec![ustr("foo")]);

        // assert
        assert_eq!(module, None);
    }

    #[test]
    fn find_registered_module() {
        // arrange
        let mut module_registry = ModuleRegistry::new();
        let mod_part = Box::new(ModPart {
            span: Span::empty(),
            path: vec![ustr("foo")],
            functions: vec![],
        });
        module_registry.register(mod_part);

        // act
        let module = module_registry.find(&vec![ustr("foo")]);

        // assert
        assert_eq!(module, Some(&Module {
            parts: vec![Box::new(ModPart{
                span: Span::empty(),
                path: vec![ustr("foo")],
                functions: vec![],
            })],
        }));
    }

    #[test]
    fn registered_module_with_two_parts() {
        // arrange
        let mut module_registry = ModuleRegistry::new();

        let mod_part1 = Box::new(ModPart {
            span: Span::empty(),
            path: vec![ustr("foo")],
            functions: vec![],
        });
        let mod_part2 = Box::new(ModPart {
            span: Span::empty(),
            path: vec![ustr("foo")],
            functions: vec![],
        });

        module_registry.register(mod_part1);
        module_registry.register(mod_part2);

        // act
        let module = module_registry.find(&vec![ustr("foo")]);

        // assert
        assert_eq!(module, Some(&Module {
            parts: vec![Box::new(ModPart{
                span: Span::empty(),
                path: vec![ustr("foo")],
                functions: vec![],
            }),Box::new(ModPart{
                span: Span::empty(),
                path: vec![ustr("foo")],
                functions: vec![],
            }),],
        }));
    }
}
