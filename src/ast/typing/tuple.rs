use crate::ast::expressions::tuple::TupleExpression;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::typing::{typecheck_expression, TypeRef};
use crate::errors::Errors;
use crate::scope::type_::TypeScope;

pub(super) fn typecheck_tuple(
    expr: &mut TupleExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    let mut types = vec![];
    for expr in &mut expr.values {
        typecheck_expression(expr, scope, errors);
        let type_ = expr.get_type(TypeCoercionHint::Any, errors);
        types.push(type_);
    }

    TypeRef::tuple(types)
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::typing::{typecheck_expression, BuiltinType, TupleType, TypeRef};
    use crate::errors::{ErrorKind, Errors};
    use crate::scope::type_::TypeScope;

    #[test]
    fn one_value() {
        // arrange
        let mut expr = Expression::tuple(0, vec![Expression::i32_literal(0, 123)]);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(
            expr.type_ref,
            Some(TypeRef::Tuple(TupleType {
                fields: vec![TypeRef::Builtin(BuiltinType::I32)],
            }))
        );
    }

    #[test]
    fn multiple_values() {
        // arrange
        let mut expr = Expression::tuple(
            0,
            vec![Expression::i32_literal(0, 1), Expression::i32_literal(0, 2)],
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(
            expr.type_ref,
            Some(TypeRef::Tuple(TupleType {
                fields: vec![
                    TypeRef::Builtin(BuiltinType::I32),
                    TypeRef::Builtin(BuiltinType::I32),
                ],
            }))
        );
    }

    #[test]
    fn unknown_value() {
        // arrange
        let mut expr = Expression::tuple(
            0,
            vec![Expression::unknown(), Expression::i32_literal(0, 1)],
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::tuple(vec![TypeRef::Unknown, TypeRef::i32()])));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn indeterminate_value() {
        // arrange
        let mut expr = Expression::tuple(
            0,
            vec![
                Expression::if_(
                    1..20,
                    Expression::bool_literal(0, true),
                    Expression::i32_literal(0, 1),
                    Some(Expression::f32_literal(0, 1.2)),
                ),
                Expression::i32_literal(0, 1),
            ],
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::tuple(vec![
            TypeRef::Unknown,
            TypeRef::i32(),
        ])));
        assert!(errors.has_error_at(1..20, ErrorKind::IndeterminateType(vec![TypeRef::i32(), TypeRef::f32()])));
    }
}
