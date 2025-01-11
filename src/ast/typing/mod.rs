mod literal;
mod binary;
mod if_;
mod block;
mod statement;

use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::typing::binary::calculate_binary_type;
use crate::ast::typing::block::calculate_block_type;
use crate::ast::typing::if_::calculate_if_type;
use crate::ast::typing::literal::calculate_literal_type;

pub struct Scope {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeRef {
    Unknown,
    Builtin(BuiltinType),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BuiltinType {
    Unit,
    I32,
}

pub fn calculate_expression_type(expr: &mut Expression, scope: &Scope) {
    let type_ref = match &mut expr.kind {
        ExpressionKind::Literal(x) => calculate_literal_type(x, scope),
        ExpressionKind::Unknown(_) => TypeRef::Unknown,
        ExpressionKind::Binary(x) => calculate_binary_type(x, scope),
        ExpressionKind::If(x) => calculate_if_type(x, scope),
        ExpressionKind::Block(x) => calculate_block_type(x, scope),
        _ => todo!(),
        // Expression::Access(x) => calculate_access_type(x, scope),
        // Expression::Compare(x) => calculate_compare_type(x, scope),
    };
    expr.type_ref = Some(type_ref);
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn unknown() {
        // arrange
        let mut expr = Expression::unknown();
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }
}