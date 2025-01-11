use crate::ast::expressions::{Expression, LiteralExpression, LiteralValue};
use crate::ast::types::Type;

pub struct Scope {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeRef {
    Unknown,
    Builtin(BuiltinType),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BuiltinType {
    I32,
}

pub fn calculate_expression_type(expr: &Expression, scope: &Scope) -> TypeRef {
    match expr {
        Expression::Literal(x) => calculate_literal_type(x, scope),
        _ => todo!(),
        // Expression::Access(x) => calculate_access_type(x, scope),
        // Expression::Block(x) => calculate_block_type(x, scope),
        // Expression::Add(x) => calculate_add_type(x, scope),
        // Expression::If(x) => calculate_if_type(x, scope),
        // Expression::Compare(x) => calculate_compare_type(x, scope),
        // Expression::Unknown(_) => TypeRef::Unknown,
    }
}

fn calculate_literal_type(expr: &LiteralExpression, scope: &Scope) -> TypeRef {
    match expr.value {
        LiteralValue::Integer(_) => TypeRef::Builtin(BuiltinType::I32),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;

    #[test]
    fn literal_i32_type() {
        // arrange
        let expr = Expression::int_literal(0, 123);
        let scope = Scope {};

        // act
        let type_ref = calculate_expression_type(&expr, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Builtin(BuiltinType::I32));
    }
}
