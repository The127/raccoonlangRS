use crate::ast::expressions::{LiteralExpression, LiteralValue};
use crate::ast::typing::{BuiltinType, Scope, TypeRef};

pub(super) fn calculate_literal_type(expr: &LiteralExpression, scope: &Scope) -> TypeRef {
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
    use crate::ast::typing::calculate_expression_type;

    #[test]
    fn i32() {
        // arrange
        let mut expr = Expression::int_literal(0, 123);
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn boolean() {
        // arrange
        let mut expr = Expression::bool_literal(0, true);
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Bool)))
    }
}
