pub mod block;
pub mod function;
pub mod module;
pub mod global;

use crate::ast::expressions::block::BlockExpression;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::scope::block::BlockScope;
use crate::ast::scope::function::FunctionScope;
use crate::ast::scope::module::ModuleScope;
use crate::ast::typing::TypeRef;
use std::collections::HashMap;
use ustr::Ustr;

pub trait Scope {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef>;

    fn module(&self) -> ModuleScope {
        ModuleScope {}
    }

    fn function(&self, func: &FunctionDecl) -> FunctionScope;

    fn block(&self, expr: &BlockExpression) -> BlockScope;
}

#[cfg(test)]
pub struct MockScope {
    types: HashMap<Ustr, TypeRef>,
}

#[cfg(test)]
impl MockScope {
    pub fn new<const N: usize>(types: [(Ustr, TypeRef); N]) -> Self {
        Self {
            types: HashMap::from(types),
        }
    }
}

#[cfg(test)]
impl Scope for MockScope {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef> {
        assert!(!rooted);
        assert_eq!(path.len(), 1);
        let name = path[0];
        self.types.get(&name)
    }

    fn function(&self, func: &FunctionDecl) -> FunctionScope {
        FunctionScope::new(self, func)
    }

    fn block(&self, expr: &BlockExpression) -> BlockScope {
        BlockScope::new(self, expr)
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::typing::BuiltinType;
    use parameterized::{ide, parameterized};
    use ustr::ustr;

    ide!();
    #[parameterized(params = {
        ("foo", Some(TypeRef::Unknown)),
        ("bar", Some(TypeRef::Builtin(BuiltinType::I32))),
        ("qux", None),
    })]
    fn mock_lookup(params: (&str, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let scope = MockScope::new([
            (ustr("foo"), TypeRef::Unknown),
            (ustr("bar"), TypeRef::Builtin(BuiltinType::I32)),
        ]);

        // act
        let got_type = scope.lookup(vec![ustr(name)], false);

        // assert
        assert_eq!(got_type, expected_type.as_ref());

    }
}