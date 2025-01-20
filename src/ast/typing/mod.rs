mod literal;
mod binary;
mod if_;
mod block;
mod statement;
mod tuple;
mod access;
pub mod function;

use crate::ast::expressions::{Expression, ExpressionKind};
use crate::scope::type_::TypeScope;
use crate::ast::typing::access::typecheck_access;
use crate::ast::typing::binary::typecheck_binary;
use crate::ast::typing::block::typecheck_block;
use crate::ast::typing::if_::typecheck_if;
use crate::ast::typing::literal::typecheck_literal;
use crate::ast::typing::tuple::typecheck_tuple;
use crate::errors::Errors;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeRef {
    Unknown,
    Builtin(BuiltinType),
    Tuple(TupleType),
}

impl TypeRef {
    pub fn tuple(fields: Vec<TypeRef>) -> TypeRef {
        TypeRef::Tuple(TupleType {
            fields,
        })
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BuiltinType {
    Unit,
    Bool,
    I32,
    U32,
    F32,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TupleType {
    pub fields: Vec<TypeRef>,
}


pub fn typecheck_expression(expr: &mut Expression, scope: &TypeScope, errors: &mut Errors) {
    let type_ref = match &mut expr.kind {
        ExpressionKind::Unknown(_) => TypeRef::Unknown,
        ExpressionKind::Literal(x) => typecheck_literal(x, scope, errors),
        ExpressionKind::Binary(x) => typecheck_binary(x, scope, errors),
        ExpressionKind::If(x) => typecheck_if(x, scope, errors),
        ExpressionKind::Block(x) => typecheck_block(x, scope, errors),
        ExpressionKind::Tuple(x) => typecheck_tuple(x, scope, errors),
        ExpressionKind::Access(x) => typecheck_access(x, scope, errors),
    };
    expr.type_ref = Some(type_ref);
}

// TODO: typechecking has to do errors when types are wrong!

#[cfg(test)]
mod test {
    use crate::scope::type_::TypeScope;
    use super::*;

    #[test]
    fn unknown() {
        // arrange
        let mut expr = Expression::unknown();
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }
}