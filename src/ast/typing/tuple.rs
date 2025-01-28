use crate::ast::expressions::tuple::TupleExpression;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::typing::{typecheck_expression};
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::TypeRef;

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
    use crate::ast::typing::{typecheck_expression};
    use crate::errors::{ErrorKind, Errors};
    use crate::scope::type_::TypeScope;
    use crate::source_map::Span;
    use crate::types::type_ref::{BuiltinType, IndeterminateTypePossibility, TupleType, TypeRef};

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
            expr.type_ref(),
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
            expr.type_ref(),
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
        assert_eq!(expr.type_ref(), Some(TypeRef::tuple(vec![TypeRef::Unknown, TypeRef::i32()])));
        errors.assert_empty();
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
                    Expression::block(1..10, vec![], Some(Expression::i32_literal(5, 1))),
                    Some(Expression::block(11..20, vec![], Some(Expression::f32_literal(15..18, 1.2)))),
                ),
                Expression::i32_literal(0, 1),
            ],
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::tuple(vec![
            TypeRef::Unknown,
            TypeRef::i32(),
        ])));
        assert!(errors.has_error_at(1..20, ErrorKind::IndeterminateType(vec![
            IndeterminateTypePossibility::new(TypeRef::i32(), vec![Span(5,6)]),
            IndeterminateTypePossibility::new(TypeRef::f32(), vec![Span(15, 18)]),
        ])));
    }
}
