use crate::ast::mod_part::ModPart;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::typing::function::{typecheck_function, typecheck_function_interior};
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
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

    pub fn typecheck(&mut self, errors: &mut Errors) {
        let scope = TypeScope::global();
        for (_, module) in &mut self.modules {
            for part in &mut module.parts {
                for func in &mut part.functions {
                    typecheck_function(func, &scope, errors);
                }
            }
        }
        
        for (_, module) in &mut self.modules {
            for part in &mut module.parts {
                for func in &mut part.functions {
                    typecheck_function_interior(func, &scope, errors);
                }
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Module {
    parts: Vec<Box<ModPart>>,
}

impl Module {
    pub fn get_function(&self, name: Ustr) -> Option<&FunctionDecl> {
        for part in &self.parts {
            for func in &part.functions {
                if func.name == Some(name) {
                    return Some(&func);
                }
            }
        }
        return None;
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::mod_part::ModPart;
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
