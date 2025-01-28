use crate::add_error;
use crate::ast::expressions::access::AccessExpression;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::source_map::HasSpan;
use crate::types::type_ref::TypeRef;

pub(super) fn typecheck_access(
    expr: &mut AccessExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    if let Some(type_ref) = scope.lookup(&expr.path) {
        type_ref.clone()
    } else {
        add_error!(errors, expr.span(), UnknownVariable(expr.path.clone()));
        TypeRef::Unknown
    }
}

#[cfg(test)]
mod test {
    use crate::ast::path::Path;
    use crate::ast::typing::{typecheck_expression};
    use crate::errors::{ErrorKind, Errors};
    use crate::scope::type_::TypeScope;
    use ustr::ustr;
    use crate::ast::expressions::Expression;
    use crate::types::type_ref::{BuiltinType, TypeRef};

    #[test]
    fn access_simple_var() {
        // arrange
        let mut expr = Expression::access(0, Path::name("foo"));
        let mut errors = Errors::new();
        let scope = TypeScope::from(&[(ustr("foo"), TypeRef::Builtin(BuiltinType::I32))]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        errors.assert_empty();
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn access_unknown_var() {
        // arrange
        let mut expr = Expression::access(0, Path::name("bar"));
        let mut errors = Errors::new();
        let scope = TypeScope::from(&[(ustr("foo"), TypeRef::Builtin(BuiltinType::I32))]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Unknown));
        assert!(errors.has_error_at(0, ErrorKind::UnknownVariable(Path::name("bar"))));
        assert_eq!(errors.get_errors().len(), 1);
    }
}
