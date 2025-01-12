use crate::ast::expressions::block::BlockExpression;
use crate::ast::typing::statement::typecheck_statement;
use crate::ast::typing::{typecheck_expression, BuiltinType, Scope, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_block(expr: &mut BlockExpression, scope: &Scope, errors: &mut Errors) -> TypeRef {
    for stmt in &mut expr.statements {
        typecheck_statement(stmt, scope, errors);
    }

    match expr.value.as_mut() {
        None => {
            TypeRef::Builtin(BuiltinType::Unit)
        }
        Some(val) => {
            typecheck_expression(val, scope, errors);
            val.type_ref.clone().unwrap()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::statement::Statement;
    use crate::ast::typing::{typecheck_expression, BuiltinType, Scope, TypeRef};
    use assert_matches::assert_matches;
    use crate::ast::expressions::block::BlockExpression;
    use crate::errors::Errors;

    #[test]
    fn empty_block() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

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
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

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
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

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
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

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
        let mut errors = Errors::new();
        let scope = Scope {};

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

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

    // TODO: typecheck let decl if there is one!
}