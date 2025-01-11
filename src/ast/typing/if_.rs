use crate::ast::expressions::IfExpression;
use crate::ast::typing::{calculate_expression_type, Scope, TypeRef};

pub(super) fn calculate_if_type(expr: &mut IfExpression, scope: &Scope) -> TypeRef {
    calculate_expression_type(expr.condition.as_mut(), scope);
    calculate_expression_type(expr.then.as_mut(), scope);
    let then_type = expr.then.type_ref.clone().unwrap();
    if let Some(else_) = expr.else_.as_mut() {
        calculate_expression_type(else_, scope);
        let else_type = else_.type_ref.clone().unwrap();

        if then_type == else_type {
            return then_type;
        }
    }
    TypeRef::Unknown
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{BlockExpression, Expression, ExpressionKind, IfExpression};
    use crate::ast::typing::{calculate_expression_type, BuiltinType, Scope, TypeRef};
    use assert_matches::assert_matches;

    #[test]
    fn if_with_else_same_type() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let else_ = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, Some(else_));

        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

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

        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }

    #[test]
    fn if_without_else() {
        // arrange
        let cond = Expression::unknown();
        let then = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let mut expr = Expression::if_(0, cond, then, None);

        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }

    #[test]
    fn condition_gets_typed() {
        // arrange
        let cond = Expression::int_literal(0, 1);
        let then = Expression::block(0, vec![], Some(Expression::unknown()));
        let mut expr = Expression::if_(0, cond, then, None);

        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

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
