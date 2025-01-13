use crate::ast::expressions::if_::IfExpression;
use crate::ast::scope::Scope;
use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_if(expr: &mut IfExpression, scope: &dyn Scope, errors: &mut Errors) -> TypeRef {
    typecheck_expression(expr.condition.as_mut(), scope, errors);
    typecheck_expression(expr.then.as_mut(), scope, errors);
    let then_type = expr.then.type_ref.clone().unwrap();
    if let Some(else_) = expr.else_.as_mut() {
        typecheck_expression(else_, scope, errors);
        let else_type = else_.type_ref.clone().unwrap();

        if then_type == else_type {
            return then_type;
        } else {
            return TypeRef::Unknown;
        }
    }

    if then_type == TypeRef::Builtin(BuiltinType::Unit) {
        then_type
    } else {
        TypeRef::Unknown
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
    use assert_matches::assert_matches;
    use crate::ast::expressions::if_::IfExpression;
    use crate::ast::scope::global::GlobalScope;
    use crate::errors::Errors;

    #[test]
    fn if_with_else_same_type() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let else_ = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn if_with_else_different_types() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], None);
        let else_ = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }

    #[test]
    fn if_with_value_without_else() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }

    #[test]
    fn if_without_value_without_else() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], None);
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
    }

    #[test]
    fn if_without_value_with_else() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], None);
        let else_ = Expression::block(0, vec![], None);
        let mut expr = Expression::if_(0, cond, then, Some(else_));
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
    }

    #[test]
    fn condition_gets_typed() {
        // arrange
        let cond = Expression::int_literal(0, 1);
        let then = Expression::block(0, vec![], Some(Expression::unknown()));
        let mut expr = Expression::if_(0, cond, then, None);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

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
    }
}
