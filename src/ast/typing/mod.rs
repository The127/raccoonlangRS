mod literal;
mod binary;
mod if_;
mod block;
mod statement;

use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::typing::binary::typecheck_binary;
use crate::ast::typing::block::typecheck_block;
use crate::ast::typing::if_::typecheck_if;
use crate::ast::typing::literal::typecheck_literal;
use crate::errors::Errors;

pub struct Scope {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeRef {
    Unknown,
    Builtin(BuiltinType),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BuiltinType {
    Unit,
    Bool,
    I32,
}

pub fn typecheck_expression(expr: &mut Expression, scope: &Scope, errors: &mut Errors) {
    let type_ref = match &mut expr.kind {
        ExpressionKind::Literal(x) => typecheck_literal(x, scope, errors),
        ExpressionKind::Unknown(_) => TypeRef::Unknown,
        ExpressionKind::Binary(x) => typecheck_binary(x, scope, errors),
        ExpressionKind::If(x) => typecheck_if(x, scope, errors),
        ExpressionKind::Block(x) => typecheck_block(x, scope, errors),
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
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }
}