use crate::ast::expressions::with::WithExpression;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::TypeRef;

pub(super) fn typecheck_with(
    expr: &mut WithExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    todo!();
}