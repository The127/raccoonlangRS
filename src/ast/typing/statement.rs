use crate::ast::statement::Statement;
use crate::ast::typing::{calculate_expression_type, Scope};

pub(super) fn calculate_statement_type(stmt: &mut Statement, scope: &Scope) {
    match stmt {
        Statement::Expression(expr) => {
            calculate_expression_type(expr, scope);
        }
        Statement::Declaration(_) => todo!()
    }
}


#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use crate::ast::expressions::Expression;
    use crate::ast::statement::Statement;
    use crate::ast::typing::{BuiltinType, Scope, TypeRef};
    use crate::ast::typing::statement::calculate_statement_type;

    #[test]
    fn expr() {
        // arrange
        let mut stmt = Statement::Expression(Expression::int_literal(0, 1));
        let scope = Scope {};

        // act
        calculate_statement_type(&mut stmt, &scope);

        // assert
        assert_matches!(stmt, Statement::Expression(Expression {
            type_ref,
            ..
        }) => {
            assert_eq!(type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        });
    }
}