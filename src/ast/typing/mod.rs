mod literal;
mod binary;

use crate::ast::expressions::Expression;
use crate::ast::typing::binary::calculate_binary_type;
use crate::ast::typing::literal::calculate_literal_type;

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
        Expression::Unknown(_) => TypeRef::Unknown,
        Expression::Binary(x) => calculate_binary_type(x, scope),
        _ => todo!(),
        // Expression::Access(x) => calculate_access_type(x, scope),
        // Expression::Block(x) => calculate_block_type(x, scope),
        // Expression::If(x) => calculate_if_type(x, scope),
        // Expression::Compare(x) => calculate_compare_type(x, scope),
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;

    #[test]
    fn unknown() {
        // arrange
        let expr = Expression::unknown();
        let scope = Scope {};

        // act
        let type_ref = calculate_expression_type(&expr, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Unknown);
    }
}