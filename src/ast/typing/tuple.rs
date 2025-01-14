use crate::ast::expressions::tuple::TupleExpression;
use crate::scope::type_::TypeScope;
use crate::ast::typing::{typecheck_expression, TupleType, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_tuple(
    expr: &mut TupleExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    let mut types = vec![];
    let mut is_unknown = false;
    for expr in &mut expr.values {
        typecheck_expression(expr, scope, errors);
        let type_ = expr.type_ref.clone().unwrap();
        if type_ == TypeRef::Unknown {
            is_unknown = true;
        }
        types.push(type_);
    }

    if is_unknown {
        TypeRef::Unknown
    } else {
        TypeRef::Tuple(TupleType {
            fields: types,
        })
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::scope::type_::TypeScope;
    use crate::ast::typing::{typecheck_expression, BuiltinType, TupleType, TypeRef};
    use crate::errors::Errors;

    #[test]
    fn one_value() {
        // arrange
        let mut expr = Expression::tuple(0, vec![
            Expression::int_literal(0, 123),
        ]);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Tuple(TupleType {
            fields: vec![
                TypeRef::Builtin(BuiltinType::I32)
            ],
        })));
    }

    #[test]
    fn multiple_values() {
        // arrange
        let mut expr = Expression::tuple(0, vec![
            Expression::int_literal(0, 1),
            Expression::int_literal(0, 2),
        ]);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Tuple(TupleType {
            fields: vec![
                TypeRef::Builtin(BuiltinType::I32),
                TypeRef::Builtin(BuiltinType::I32),
            ],
        })));
    }

    #[test]
    fn unknown_value() {
        // arrange
        let mut expr = Expression::tuple(0, vec![
            Expression::unknown(),
            Expression::int_literal(0, 1),
        ]);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
        assert_matches!(expr.kind, ExpressionKind::Tuple(x) => {
            assert_matches!(x.values[..], [
                Expression {
                   type_ref: Some(TypeRef::Unknown),
                    ..
                },
                 Expression {
                   type_ref: Some(TypeRef::Builtin(BuiltinType::I32)),
                    ..
                },
            ]);
        })
    }
}