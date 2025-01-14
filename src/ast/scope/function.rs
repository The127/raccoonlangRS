use crate::ast::expressions::block::BlockExpression;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::scope::block::BlockScope;
use crate::ast::scope::Scope;
use crate::ast::typing::TypeRef;
use ustr::{Ustr, UstrMap};

pub struct FunctionScope<'a> {
    params: UstrMap<TypeRef>,
    parent: &'a dyn Scope,
}

impl FunctionScope<'_> {
    pub fn new<'a>(parent: &'a dyn Scope, func: &FunctionDecl) -> FunctionScope<'a> {
        let mut params = UstrMap::default();

        for param in &func.parameters {
            params.insert(param.name, param.type_ref.clone().unwrap());
        }

        FunctionScope { parent, params }
    }
}

impl Scope for FunctionScope<'_> {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef> {
        assert!(!rooted);
        assert_eq!(path.len(), 1);
        let name = path[0];

        self.params
            .get(&name)
            .or_else(|| self.parent.lookup(path, rooted))
    }

    fn function(&self, func: &FunctionDecl) -> FunctionScope {
        todo!()
    }

    fn block(&self, expr: &BlockExpression) -> BlockScope {
        BlockScope::new(self, expr)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
    use crate::ast::scope::MockScope;
    use crate::ast::types::{NamedType, Type};
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ast::Visibility;
    use parameterized::{ide, parameterized};
    use ustr::ustr;

    ide!();

    #[parameterized(params = {
        (ustr("foo"), Some(TypeRef::Builtin(BuiltinType::I32))),
        (ustr("bar"), None),
        (ustr("qux"), Some(TypeRef::Builtin(BuiltinType::Bool))),
    })]
    fn function_scope_param(params: (Ustr, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let global_scope = MockScope::new([(ustr("qux"), TypeRef::Builtin(BuiltinType::Bool))]);
        let mut func = FunctionDecl::new(
            0,
            None,
            Visibility::Module,
            vec![FunctionParameter::new(
                0,
                ustr("foo"),
                Type::Named(NamedType::new(0, vec![ustr("i32")], false)),
            )
            .with_type_ref(TypeRef::Builtin(BuiltinType::I32))],
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
