mod access;
mod binary;
mod block;
pub mod function;
mod if_;
mod literal;
mod statement;
mod struct_;
mod tuple;
mod dot_access;
mod call;

use crate::add_error;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::types::Type;
use crate::ast::typing::access::typecheck_access;
use crate::ast::typing::binary::typecheck_binary;
use crate::ast::typing::block::typecheck_block;
use crate::ast::typing::if_::typecheck_if;
use crate::ast::typing::literal::typecheck_literal;
use crate::ast::typing::tuple::typecheck_tuple;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::{BuiltinType, TypeRef};
use std::collections::HashSet;
use crate::ast::typing::call::typecheck_call;
use crate::ast::typing::dot_access::typecheck_dot_access;
use crate::source_map::HasSpan;

pub fn map_type(type_: &Type, errors: &mut Errors, scope: &TypeScope) -> TypeRef {
    match type_ {
        Type::Unknown => TypeRef::Unknown,
        Type::Unit => TypeRef::Builtin(BuiltinType::Unit),
        Type::Named(named_type) => {
            let type_ref = scope.lookup(&named_type.path);
            if let Some(t) = type_ref {
                t.clone()
            } else {
                add_error!(
                    errors,
                    named_type.span(),
                    UnknownType(named_type.path.clone())
                );
                TypeRef::Unknown
            }
        }
    }
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
        ExpressionKind::DotAccess(x) => typecheck_dot_access(x, scope, errors),
        ExpressionKind::Call(x) => typecheck_call(x, scope, errors),
        ExpressionKind::Index(x) => todo!(),
        ExpressionKind::With(x) => todo!(),
    };
    expr.set_type_ref(type_ref);
}

// TODO: typechecking has to do errors when types are wrong!

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::path::Path;
    use crate::ast::types::NamedType;
    use crate::errors::ErrorKind;
    use crate::scope::type_::TypeScope;
    use parameterized::parameterized;

    #[test]
    fn map_unit() {
        // arrange
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&Type::Unit, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Builtin(BuiltinType::Unit));
        errors.assert_empty();
    }

    #[test]
    fn map_unknown() {
        // arrange
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&Type::Unknown, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Unknown);
    }

    #[parameterized(params = {
        ("i32", TypeRef::Builtin(BuiltinType::I32)),
        ("u32", TypeRef::Builtin(BuiltinType::U32)),
        ("f32", TypeRef::Builtin(BuiltinType::F32)),
        ("bool", TypeRef::Builtin(BuiltinType::Bool)),
    })]
    fn map_type_named_builtin(params: (&str, TypeRef)) {
        let (name, expected) = params;
        // arrange
        let type_ = Type::Named(NamedType::new(0, Path::name(name)));
        let scope = TypeScope::global();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&type_, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, expected);
        errors.assert_empty();
    }

    #[test]
    fn map_unknown_named_type() {
        // arrange
        let type_ = Type::Named(NamedType::new(7..10, Path::name("foo")));
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&type_, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Unknown);
        assert!(errors.has_error_at(7..10, ErrorKind::UnknownType(Path::name("foo"))));
        assert_eq!(errors.get_errors().len(), 1);
    }

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
}
