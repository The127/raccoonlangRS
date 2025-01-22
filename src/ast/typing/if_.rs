use crate::ast::expressions::if_::IfExpression;
use crate::scope::type_::TypeScope;
use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
use crate::errors::{ErrorKind, Errors};
use crate::source_map::HasSpan;

pub(super) fn typecheck_if(expr: &mut IfExpression, scope: &TypeScope, errors: &mut Errors) -> TypeRef {
    typecheck_expression(expr.condition.as_mut(), scope, errors);
    let cond_type = expr.condition.type_ref.as_ref().unwrap();
    if cond_type != &TypeRef::Builtin(BuiltinType::Bool) {
        errors.add(ErrorKind::TypeMismatch(cond_type.clone(), TypeRef::Builtin(BuiltinType::Bool)), expr.condition.span())
    }

    typecheck_expression(expr.then.as_mut(), scope, errors);
    let then_type = expr.then.type_ref.clone().unwrap();
    let else_type = if let Some(else_) = expr.else_.as_mut() {
        typecheck_expression(else_, scope, errors);
        else_.type_ref.clone().unwrap()
    } else {
        TypeRef::Builtin(BuiltinType::Unit)
    };

    TypeRef::merge_indeterminate(then_type, else_type)
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
    use assert_matches::assert_matches;
    use crate::ast::expressions::if_::IfExpression;
    use crate::scope::type_::TypeScope;
    use crate::errors::{ErrorKind, Errors};

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
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn if_with_else_different_types() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(0, vec![], Some(Expression::f32_literal(0, 1.2)));
        let else_ = Expression::block(0, vec![], Some(Expression::i32_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Indeterminate(vec![TypeRef::Builtin(BuiltinType::F32), TypeRef::Builtin(BuiltinType::I32)])));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn if_else_chain_different_types_with_duplicate() {
        let cond = Expression::bool_literal(0, false);
        let mut expr = Expression::if_(
            0,
            cond.clone(),
            Expression::i32_literal(0, 1),
            Some(Expression::if_(
                0,
                cond.clone(),
                Expression::f32_literal(0, 1.0),
                Some(Expression::if_(
                    0,
                    cond,
                    Expression::u32_literal(0, 1),
                    Some(Expression::i32_literal(0, -1))
                ))
            ))
        );

        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Indeterminate(vec![
            TypeRef::Builtin(BuiltinType::I32),
            TypeRef::Builtin(BuiltinType::F32),
            TypeRef::Builtin(BuiltinType::U32),
        ])));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn if_with_value_without_else() {
        // arrange
        let cond = Expression::bool_literal(0, true);
        let then = Expression::block(0, vec![], Some(Expression::i32_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Indeterminate(vec![TypeRef::Builtin(BuiltinType::I32), TypeRef::Builtin(BuiltinType::Unit)])));
        assert!(errors.get_errors().is_empty());
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
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
        assert!(errors.get_errors().is_empty());
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
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
        assert!(errors.get_errors().is_empty());
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
            assert_eq!(condition.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        });
        assert!(errors.has_error_at(2, ErrorKind::TypeMismatch(TypeRef::Builtin(BuiltinType::I32), TypeRef::Builtin(BuiltinType::Bool))));
        assert_eq!(errors.get_errors().len(), 1);
    }
}
