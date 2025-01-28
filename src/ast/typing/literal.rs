use crate::ast::expressions::literal::{LiteralExpression, LiteralValue};
use crate::scope::type_::TypeScope;
use crate::types::type_ref::{BuiltinType, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_literal(expr: &LiteralExpression, scope: &TypeScope, errors: &mut Errors) -> TypeRef {
    match expr.value {
        LiteralValue::I32(_) => TypeRef::Builtin(BuiltinType::I32),
        LiteralValue::U32(_) => TypeRef::Builtin(BuiltinType::U32),
        LiteralValue::F32(_) => TypeRef::Builtin(BuiltinType::F32),
        LiteralValue::Boolean(_) => TypeRef::Builtin(BuiltinType::Bool),
    }
}


#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::typing::typecheck_expression;
    use super::*;
    use crate::scope::type_::TypeScope;

    #[test]
    fn i32() {
        // arrange
        let mut expr = Expression::i32_literal(0, 123);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn u32() {
        // arrange
        let mut expr = Expression::u32_literal(0, 123);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::U32)));
    }

    #[test]
    fn f32() {
        // arrange
        let mut expr = Expression::f32_literal(0, 1.23);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::F32)));
    }

    #[test]
    fn boolean() {
        // arrange
        let mut expr = Expression::bool_literal(0, true);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::Bool)))
    }
}
