use crate::ast::expressions::BlockExpression;
use crate::ast::statement::Statement;
use crate::ast::typing::{calculate_expression_type, BuiltinType, Scope, TypeRef};
use crate::ast::typing::statement::calculate_statement_type;

pub(super) fn calculate_block_type(expr: &mut BlockExpression, scope: &Scope) -> TypeRef {
    for stmt in &mut expr.statements {
        calculate_statement_type(stmt, scope);
    }

    match expr.value.as_mut() {
        None => {
            TypeRef::Builtin(BuiltinType::Unit)
        }
        Some(val) => {
            calculate_expression_type(val, scope);
            val.type_ref.clone().unwrap()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{BlockExpression, Expression, ExpressionKind};
    use crate::ast::statement::Statement;
    use crate::ast::typing::{calculate_expression_type, BuiltinType, Scope, TypeRef};
    use assert_matches::assert_matches;

    #[test]
    fn empty_block() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
    }

    #[test]
    fn statements_no_value() {
        // arrange
        let mut expr = Expression::block(0, vec![
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::unknown()),
        ], None);
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
    }

    #[test]
    fn i32_value() {
        // arrange
        let mut expr = Expression::block(0, vec![
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::unknown()),
        ], Some(Expression::int_literal(0, 123)));
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn unknown_value() {
        // arrange
        let mut expr = Expression::block(0, vec![
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::unknown()),
        ], Some(Expression::unknown()));
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Unknown));
    }

    #[test]
    fn calculate_statement_types() {
        // arrange

        let mut expr = Expression::block(0, vec![
            Statement::Expression(Expression::unknown()),
            Statement::Expression(Expression::int_literal(0, 123)),
        ], None);
        let scope = Scope {};

        // act
        calculate_expression_type(&mut expr, &scope);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
        assert_matches!(expr.kind, ExpressionKind::Block(BlockExpression {
            statements,
            ..
        }) => {
            assert_matches!(&statements[..], [
                Statement::Expression(s1),
                Statement::Expression(s2),
            ] => {
                assert_eq!(s1.type_ref, Some(TypeRef::Unknown));
                assert_eq!(s2.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
            })
        });
    }
}