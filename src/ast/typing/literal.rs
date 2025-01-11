use crate::ast::expressions::{LiteralExpression, LiteralValue};
use crate::ast::typing::{BuiltinType, Scope, TypeRef};

pub(super) fn calculate_literal_type(expr: &LiteralExpression, scope: &Scope) -> TypeRef {
    match expr.value {
        LiteralValue::Integer(_) => TypeRef::Builtin(BuiltinType::I32),
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
        let expr = Expression::int_literal(0, 123);
        let scope = Scope {};

        // act
        let type_ref = calculate_expression_type(&expr, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Builtin(BuiltinType::I32));
    }
}
