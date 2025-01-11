use crate::ast::expressions::{BinaryExpression, BinaryOperator};
use crate::ast::typing::{calculate_expression_type, BuiltinType, Scope, TypeRef};

pub(super) fn calculate_binary_type(expr: &BinaryExpression, scope: &Scope) -> TypeRef {
    let left_type = calculate_expression_type(expr.left.as_ref(), scope);
    let right_type = calculate_expression_type(expr.right.as_ref(), scope);

    if left_type == TypeRef::Unknown || right_type == TypeRef::Unknown {
        return TypeRef::Unknown;
    }

    match expr.op {
        BinaryOperator::Plus|BinaryOperator::Minus => TypeRef::Builtin(BuiltinType::I32),
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use parameterized::parameterized;
    use super::*;
    use crate::ast::expressions::{AddExpressionOperator, BinaryOperator, Expression};
    use crate::ast::typing::{calculate_expression_type, BuiltinType};

    #[parameterized(op = {BinaryOperator::Plus, BinaryOperator::Minus})]
    fn addsub_i32_and_i32(op: BinaryOperator) {
        // arrange
        let expr = Expression::binary(
            0,
            op,
            Expression::int_literal(0, 1),
            Expression::int_literal(0, 2),
        );
        let scope = Scope {};

        // act
        let type_ref = calculate_expression_type(&expr, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Builtin(BuiltinType::I32));
    }

    #[test]
    fn add_i32_and_unknown() {
        // arrange
        let expr = Expression::binary(
            0,
            BinaryOperator::Plus,
            Expression::int_literal(0, 1),
            Expression::unknown(),
        );
        let scope = Scope {};

        // act
        let type_ref = calculate_expression_type(&expr, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Unknown);
    }
}
