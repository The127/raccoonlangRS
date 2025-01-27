mod access;
mod binary;
mod block;
pub mod function;
mod if_;
mod literal;
mod statement;
mod tuple;

use std::collections::HashSet;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::typing::access::typecheck_access;
use crate::ast::typing::binary::typecheck_binary;
use crate::ast::typing::block::typecheck_block;
use crate::ast::typing::if_::typecheck_if;
use crate::ast::typing::literal::typecheck_literal;
use crate::ast::typing::tuple::typecheck_tuple;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TypeRef {
    Unknown,
    Builtin(BuiltinType),
    Tuple(TupleType),
    Indeterminate(Vec<TypeRef>),
}

impl TypeRef {
    pub const fn unit() -> Self {
        Self::Builtin(BuiltinType::Unit)
    }

    pub const fn bool() -> Self {
        Self::Builtin(BuiltinType::Bool)
    }

    pub const fn i32() -> Self {
        Self::Builtin(BuiltinType::I32)
    }

    pub const fn u32() -> Self {
        Self::Builtin(BuiltinType::U32)
    }

    pub const fn f32() -> Self {
        Self::Builtin(BuiltinType::F32)
    }

    pub const fn tuple(fields: Vec<TypeRef>) -> Self {
        Self::Tuple(TupleType { fields })
    }

    pub fn merge_indeterminate(t1: Self, t2: Self) -> Self {
        match (t1, t2) {
            (TypeRef::Unknown, _) | (_, TypeRef::Unknown) => TypeRef::Unknown,
            (TypeRef::Indeterminate(p1), TypeRef::Indeterminate(p2)) => {
                TypeRef::Indeterminate(Self::merge_typevecs(p1, p2))
            },
            (TypeRef::Indeterminate(p), other) => {
                TypeRef::Indeterminate(Self::merge_typevecs(p, vec![other]))
            },
            (this, TypeRef::Indeterminate(p)) => {
                TypeRef::Indeterminate(Self::merge_typevecs(vec![this], p))
            }
            (t1, t2) if t1 == t2 => t1,
            (t1, t2) => TypeRef::Indeterminate(vec![t1, t2]),
        }
    }

    fn merge_typevecs(v1: Vec<TypeRef>, v2: Vec<TypeRef>) -> Vec<TypeRef> {
        let mut result = Vec::with_capacity(v1.len() + v2.len());
        let v1_set: HashSet<TypeRef> = HashSet::from_iter(v1.iter().cloned());
        result.extend(v1);
        for t in &v2 {
            if !v1_set.contains(t) {
                result.push(t.clone());
            }
        }
        result
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum BuiltinType {
    Unit,
    Bool,
    I32,
    U32,
    F32,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
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
        ExpressionKind::DotAccess(x) => todo!(),
        ExpressionKind::Call(x) => todo!(),
        ExpressionKind::Index(x) => todo!(),
        ExpressionKind::With(x) => todo!(),
    };
    expr.set_expression(type_ref);
}

// TODO: typechecking has to do errors when types are wrong!

#[cfg(test)]
mod test {
    use parameterized::parameterized;
    use super::*;
    use crate::scope::type_::TypeScope;

    #[test]
    fn typecheck_unknown() {
        // arrange
        let mut expr = Expression::unknown();
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Unknown));
    }

    #[parameterized(params = {
        (TypeRef::Unknown, TypeRef::Unknown, TypeRef::Unknown),
        (TypeRef::Unknown, TypeRef::unit(), TypeRef::Unknown),
        (TypeRef::Indeterminate(vec![TypeRef::unit(), TypeRef::i32()]), TypeRef::Unknown, TypeRef::Unknown),
        (TypeRef::unit(), TypeRef::Unknown, TypeRef::Unknown),
        (TypeRef::unit(), TypeRef::unit(), TypeRef::unit()),
        (TypeRef::i32(), TypeRef::u32(), TypeRef::Indeterminate(vec![TypeRef::i32(), TypeRef::u32()])),
        (TypeRef::Indeterminate(vec![TypeRef::unit(), TypeRef::i32()]), TypeRef::u32(), TypeRef::Indeterminate(vec![TypeRef::unit(), TypeRef::i32(), TypeRef::u32()])),
    })]
    fn merge_indeterminate(params: (TypeRef, TypeRef, TypeRef)) {
        let (left, right, expected) = params;
        // arrange

        // act
        let result = TypeRef::merge_indeterminate(left, right);

        // assert
        assert_eq!(result, expected);
    }
}
