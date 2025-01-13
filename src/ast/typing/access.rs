use crate::ast::expressions::access::AccessExpression;
use crate::ast::scope::Scope;
use crate::ast::typing::TypeRef;
use crate::errors::Errors;

pub(super) fn typecheck_access(
    expr: &mut AccessExpression,
    scope: &dyn Scope,
    errors: &mut Errors,
) -> TypeRef {
    if let Some(type_ref) = scope.lookup(vec![expr.name], false) {
        type_ref.clone()
    } else {
        TypeRef::Unknown
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::scope::MockScope;
    use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
    use crate::errors::Errors;
    use ustr::ustr;

    #[test]
    fn access_simple_var() {
        // arrange
        let mut expr = Expression::access(0, ustr("foo"));
        let mut errors = Errors::new();
        let scope = MockScope::new([(ustr("foo"), TypeRef::Builtin(BuiltinType::I32))]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert!(errors.get_errors().is_empty());
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn access_unknown_var() {
        // arrange
        let mut expr = Expression::access(0, ustr("bar"));
        let mut errors = Errors::new();
        let scope = MockScope::new([(ustr("foo"), TypeRef::Builtin(BuiltinType::I32))]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert!(errors.get_errors().is_empty());
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }
}