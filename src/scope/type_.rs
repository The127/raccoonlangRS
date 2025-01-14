use ustr::UstrMap;
use crate::ast::expressions::block::BlockExpression;
use crate::ast::function_decl::FunctionDecl;
use crate::scope::Scope;
use crate::ast::typing::TypeRef;

pub type TypeScope<'a> = Scope<'a, TypeRef>;

impl TypeScope<'_> {
    pub fn global() -> Self {
        Self::new()
    }

    pub fn function(&self, func: &FunctionDecl) -> TypeScope {
        let mut values = UstrMap::default();

        for param in &func.parameters {
            values.insert(param.name, param.type_ref.clone().unwrap());
        }
        TypeScope {
            parent: Some(self),
            values: values,
        }
    }

    pub fn block(&self, block: &BlockExpression) -> TypeScope {
        let decl = block
            .let_
            .as_ref()
            .expect("only create BlockScope for blocks that have a decl");

        let mut values = UstrMap::default();
        values.insert(decl.binding, decl.type_ref.clone().expect("decl must already have been typechecked"));

        TypeScope {
            parent: Some(self),
            values: values,
        }
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use ustr::ustr;
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::function_decl::{FunctionParameter, FunctionReturnType};
    use crate::ast::types::{NamedType, Type};
    use crate::ast::typing::BuiltinType;
    use crate::ast::Visibility;
    use super::*;

    #[test]
    fn function() {
        // arrange
        let global = TypeScope::global();
        let func_decl = FunctionDecl::new(
            0,
            None,
            Visibility::Module,
            vec![
                FunctionParameter::new(
                0,
                ustr("foo"),
                Type::Named(NamedType::new(0, vec![ustr("i32")], false)),
            )
                .with_type_ref(TypeRef::Builtin(BuiltinType::I32)),
                FunctionParameter::new(
                0,
                ustr("bar"),
                Type::Named(NamedType::new(0, vec![ustr("bool")], false)),
            )
                .with_type_ref(TypeRef::Builtin(BuiltinType::Bool)),
            ],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: Some(TypeRef::Builtin(BuiltinType::Unit)),
            },
            Expression::block(0, vec![], None),
        );


        // act
        let func_scope = global.function(&func_decl);

        // assert
        assert_eq!(func_scope.values.len(), 2);
        assert_eq!(func_scope.values.get(&ustr("foo")), Some(&TypeRef::Builtin(BuiltinType::I32)));
        assert_eq!(func_scope.values.get(&ustr("bar")), Some(&TypeRef::Builtin(BuiltinType::Bool)));
    }

    #[test]
    fn block() {
        // arrange
        let global = TypeScope::global();
        let expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1)))
                .with_type_ref(TypeRef::Builtin(BuiltinType::I32)),
            vec![],
            None,
        );
        let block_expr = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

        // act
        let block_scope = global.block(&block_expr);

        // assert
        assert_eq!(block_scope.values.len(), 1);
        assert_eq!(block_scope.values.get(&ustr("foo")), Some(&TypeRef::Builtin(BuiltinType::I32)));
    }

}