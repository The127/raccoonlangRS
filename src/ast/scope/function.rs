use ustr::{Ustr, UstrMap};
use crate::ast::expressions::block::BlockExpression;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::scope::block::BlockScope;
use crate::ast::scope::Scope;
use crate::ast::typing::TypeRef;

pub struct FunctionScope {
    params: UstrMap<TypeRef>
}

impl FunctionScope {
    pub fn new(func: &FunctionDecl) -> Self {
        let mut params = UstrMap::default();

        for param in &func.parameters {
            params.insert(param.name, param.type_ref.clone().unwrap());
        }

        Self {
            params,
        }
    }
}

impl Scope for FunctionScope {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef> {
        assert!(!rooted);
        assert_eq!(path.len(), 1);
        let name = path[0];

        self.params.get(&name)
    }

    fn block(&self, expr: &BlockExpression) -> BlockScope {
        todo!()
    }
}


#[cfg(test)]
mod test {
    use parameterized::{ide, parameterized};
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
    use crate::ast::types::{NamedType, Type};
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ast::Visibility;
    use ustr::ustr;
    use crate::ast::scope::global::GlobalScope;

    ide!();

    #[parameterized(params = {
        (ustr("foo"), Some(TypeRef::Builtin(BuiltinType::I32))),
        (ustr("bar"), None)
    })]
    fn function_scope_param(params: (Ustr, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let global_scope = GlobalScope::new();
        let mut func = FunctionDecl::new(
            0,
            None,
            Visibility::Module,
            vec![FunctionParameter::new(
                0,
                ustr("foo"),
                Type::Named(NamedType::new(0, vec![ustr("i32")], false)),
            ).with_type_ref(TypeRef::Builtin(BuiltinType::I32))],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: Some(TypeRef::Builtin(BuiltinType::Unit)),
            },
            Expression::block(0, vec![], None),
        );

        // act
        let func_scope = global_scope.function(&func);
        let got_type = func_scope.lookup(vec![name], false);

        // assert
        assert_eq!(got_type, expected_type.as_ref());
    }
}
