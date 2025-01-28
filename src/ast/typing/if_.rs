use crate::ast::expressions::if_::IfExpression;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::typing::typecheck_expression;
use crate::errors::Errors;
use crate::parser::ToSpanned;
use crate::scope::type_::TypeScope;
use crate::source_map::HasSpan;
use crate::types::type_ref::{BuiltinType, TypeRef};

pub(super) fn typecheck_if(
    expr: &mut IfExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    typecheck_expression(expr.condition.as_mut(), scope, errors);
    // we want the side effect of the error
    expr.condition.get_type(
        TypeCoercionHint::Specific(TypeRef::Builtin(BuiltinType::Bool)),
        errors,
    );

    typecheck_expression(expr.then.as_mut(), scope, errors);
    let then_type = expr.then.get_type(TypeCoercionHint::NoCoercion, errors);
    let then_span = expr.then.value_span(); // TODO: test
    let (else_type, else_span) = if let Some(else_) = expr.else_.as_mut() {
        typecheck_expression(else_, scope, errors);
        (
            else_.get_type(TypeCoercionHint::NoCoercion, errors),
            else_.value_span(),
        )
    } else {
        (
            TypeRef::Builtin(BuiltinType::Unit),
            expr.then.span().end().into(),
        )
    };

    TypeRef::merge_indeterminate(then_type.spanned(then_span), else_type.spanned(else_span))
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::if_::IfExpression;
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::typing::typecheck_expression;
    use crate::errors::{ErrorKind, Errors};
    use crate::scope::type_::TypeScope;
    use crate::source_map::Span;
    use crate::types::type_ref::{BuiltinType, IndeterminateTypePossibility, TypeRef};
    use assert_matches::assert_matches;

    #[test]
    fn if_with_else_same_type() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(0, vec![], Some(Expression::i32_literal(0, 1)));
        let else_ = Expression::block(0, vec![], Some(Expression::i32_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
        errors.assert_empty();
    }

    #[test]
    fn if_with_else_different_types() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(1..5, vec![], Some(Expression::f32_literal(2..4, 1.2)));
        let else_ = Expression::block(10..15, vec![], Some(Expression::i32_literal(13, 1)));
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(
            expr.type_ref(),
            Some(TypeRef::Indeterminate(vec![
                IndeterminateTypePossibility::new(
                    TypeRef::Builtin(BuiltinType::F32),
                    vec![Span(2, 4)]
                ),
                IndeterminateTypePossibility::new(
                    TypeRef::Builtin(BuiltinType::I32),
                    vec![Span(13, 14)]
                ),
            ]))
        );
        errors.assert_empty();
    }

    #[test]
    fn if_else_chain_different_types_with_duplicate() {
        let cond = Expression::bool_literal(0, false);
        let mut expr = Expression::if_(
            0,
            cond.clone(),
            Expression::block(1..10, vec![], Some(Expression::i32_literal(5, 1))),
            Some(Expression::if_(
                0,
                cond.clone(),
                Expression::block(11..20, vec![], Some(Expression::f32_literal(15..18, 1.0))),
                Some(Expression::if_(
                    0,
                    cond,
                    Expression::block(21..30, vec![], Some(Expression::u32_literal(25, 1))),
                    Some(Expression::block(
                        31..40,
                        vec![],
                        Some(Expression::i32_literal(35..37, -1)),
                    )),
                )),
            )),
        );

        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(
            expr.type_ref(),
            Some(TypeRef::Indeterminate(vec![
                IndeterminateTypePossibility::new(
                    TypeRef::Builtin(BuiltinType::I32),
                    vec![Span(5, 6), Span(35, 37)]
                ),
                IndeterminateTypePossibility::new(
                    TypeRef::Builtin(BuiltinType::F32),
                    vec![Span(15, 18)]
                ),
                IndeterminateTypePossibility::new(
                    TypeRef::Builtin(BuiltinType::U32),
                    vec![Span(25, 26)]
                ),
            ]))
        );
        errors.assert_empty();
    }

    #[test]
    fn if_with_value_without_else() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(1..10, vec![], Some(Expression::i32_literal(5, 1)));
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(
            expr.type_ref(),
            Some(TypeRef::Indeterminate(vec![
                IndeterminateTypePossibility::new(TypeRef::Builtin(BuiltinType::I32), vec![Span(5,6)]),
                IndeterminateTypePossibility::new(TypeRef::Builtin(BuiltinType::Unit), vec![Span(10, 11)]),
            ]))
        );
        errors.assert_empty();
    }

    #[test]
    fn if_without_value_without_else() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(0, vec![], None);
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::Unit)));
        errors.assert_empty();
    }

    #[test]
    fn if_without_values_with_else() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(0, vec![], None);
        let else_ = Expression::block(0, vec![], None);
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::Unit)));
        errors.assert_empty();
    }

    #[test]
    fn condition_gets_checked() {
        // arrange
        let cond = Expression::i32_literal(2, 1);
        let then = Expression::block(0, vec![], Some(Expression::unknown()));
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(expr, Expression {
            kind: ExpressionKind::If(
                IfExpression {
                    condition,
                    ..
                }),
            ..
        } => {
            assert_eq!(condition.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
        });
        assert!(errors.has_error_at(
            2,
            ErrorKind::TypeMismatch(
                TypeRef::Builtin(BuiltinType::I32),
                TypeRef::Builtin(BuiltinType::Bool)
            )
        ));
        assert_eq!(errors.get_errors().len(), 1);
    }
}
