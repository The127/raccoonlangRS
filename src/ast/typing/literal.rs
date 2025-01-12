use crate::ast::expressions::{LiteralExpression, LiteralValue};
use crate::ast::typing::{BuiltinType, Scope, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_literal(expr: &LiteralExpression, scope: &Scope, errors: &mut Errors) -> TypeRef {
    match expr.value {
        LiteralValue::Integer(_) => TypeRef::Builtin(BuiltinType::I32),
        LiteralValue::Boolean(_) => TypeRef::Builtin(BuiltinType::Bool),
        _ => todo!()
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ast::typing::typecheck_expression;

    #[test]
    fn i32() {
        // arrange
        let mut expr = Expression::int_literal(0, 123);
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn boolean() {
        // arrange
        let mut expr = Expression::bool_literal(0, true);
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Bool)))
    }
}
