use crate::ast::expressions::{BinaryExpression, BinaryOperator};
use crate::ast::typing::{typecheck_expression, BuiltinType, Scope, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_binary(expr: &mut BinaryExpression, scope: &Scope, errors: &mut Errors) -> TypeRef {
    typecheck_expression(expr.left.as_mut(), scope, errors);
    typecheck_expression(expr.right.as_mut(), scope, errors);

    let left_type = expr.left.type_ref.as_ref().unwrap();
    let right_type = expr.right.type_ref.as_ref().unwrap();

    if left_type == &TypeRef::Unknown || right_type == &TypeRef::Unknown {
        return TypeRef::Unknown;
    }

    match expr.op {
        BinaryOperator::Plus | BinaryOperator::Minus => TypeRef::Builtin(BuiltinType::I32),
        BinaryOperator::Equals
        | BinaryOperator::NotEquals
        | BinaryOperator::GreaterThan
        | BinaryOperator::LessThan
        | BinaryOperator::GreaterThanOrEquals
        | BinaryOperator::LessThanOrEquals => TypeRef::Builtin(BuiltinType::Bool),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::{
        BinaryOperator, Expression,
    };
    use crate::ast::typing::{typecheck_expression, BuiltinType};
    use parameterized::parameterized;

    #[parameterized(op = {BinaryOperator::Plus, BinaryOperator::Minus})]
    fn addsub_i32_and_i32(op: BinaryOperator) {
        // arrange
        let mut expr = Expression::binary(
            0,
            op,
            Expression::int_literal(0, 1),
            Expression::int_literal(0, 2),
        );
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn add_i32_and_unknown() {
        // arrange
        let mut expr = Expression::binary(
            0,
            BinaryOperator::Plus,
            Expression::int_literal(0, 1),
            Expression::unknown(),
        );
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }
}
