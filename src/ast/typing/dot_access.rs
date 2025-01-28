use crate::ast::expressions::dot_access::DotAccessExpression;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::typing::typecheck_expression;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::TypeRef;

pub(super) fn typecheck_dot_access(
    expr: &mut DotAccessExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    typecheck_expression(expr.target.as_mut(), scope, errors);

    let target_type = expr.target.get_type(TypeCoercionHint::Any, errors);

    let member_type = target_type.get_member_type(expr.name.value).unwrap();

    member_type
}

#[cfg(test)]
mod test {
    use crate::refs::MutableRef;
    use crate::types::type_ref::{BuiltinType, StructType, StructTypeMember, TypeRef};
    use ustr::ustr;
    use crate::ast::expressions::Expression;
    use crate::ast::path::Path;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::parser::ToSpanned;
    use crate::scope::type_::TypeScope;

    #[test]
    fn struct_access() {
        // arrange
        let struct_type_ref = MutableRef::<StructType>::default();
        {
            let mut struct_type = struct_type_ref.borrow_mut();
            struct_type.members = vec![StructTypeMember::new(
                ustr("bar"),
                TypeRef::Builtin(BuiltinType::I32),
            )];
        }
        let type_ref = TypeRef::Struct(struct_type_ref.get_immutable());

        let scope = TypeScope::from(&[(ustr("foo_value"), type_ref.clone())]);
        let mut errors = Errors::new();

        let mut expr = Expression::dot_access(0, Expression::access(0, Path::name("foo_value")), ustr("bar").spanned_empty());

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
        errors.assert_empty();
    }

    // #[test]
    fn unknown_member() {
        todo!()
    }
}
