use crate::ast::file::ModPart;
use std::collections::HashMap;
use ustr::Ustr;
use crate::ast::typing::{typecheck_expression};
use crate::errors::Errors;

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

    pub fn typecheck(&mut self, errors: &mut Errors) {
        for (_, module) in &mut self.modules {
            for part in &mut module.parts {
                for func in &mut part.functions {
                    let scope = crate::ast::typing::Scope {};
                    typecheck_expression(&mut func.body, &scope, errors);
                }
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Module {
    parts: Vec<Box<ModPart>>,
}

pub trait Scope {
    fn lookup(&self, path: Vec<Ustr>) -> Option<()>; // TODO
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::file::ModPart;
    use ustr::ustr;

    #[test]
    fn find_unknown_module() {
        // arrange
        let module_registry = ModuleRegistry::new();

        // act
        let module = module_registry.find(&vec![ustr("foo")]);

        // assert
        assert_eq!(module, None);
    }

    #[test]
    fn find_registered_module() {
        // arrange
        let mut module_registry = ModuleRegistry::new();
        let mod_part = Box::new(ModPart::new(0..0, vec![ustr("foo")]));
        module_registry.register(mod_part.clone());

        // act
        let module = module_registry.find(&vec![ustr("foo")]);

        // assert
        assert_eq!(module, Some(&Module {
            parts: vec![mod_part],
        }));
    }

    #[test]
    fn registered_module_with_two_parts() {
        // arrange
        let mut module_registry = ModuleRegistry::new();

        let mod_part1 = Box::new(ModPart::new(0..0, vec![ustr("foo")]));
        let mod_part2 = Box::new(ModPart::new(0..0, vec![ustr("foo")]));

        module_registry.register(mod_part1.clone());
        module_registry.register(mod_part2.clone());

        // act
        let module = module_registry.find(&vec![ustr("foo")]);

        // assert
        assert_eq!(module, Some(&Module {
            parts: vec![mod_part1, mod_part2],
        }));
    }
}
