use crate::scope::type_::TypeScope;
use crate::ast::statement::Statement;
use crate::ast::typing::{typecheck_expression};
use crate::errors::Errors;

pub(super) fn typecheck_statement(stmt: &mut Statement, scope: &TypeScope, errors: &mut Errors) {
    match stmt {
        Statement::Expression(expr) => {
            typecheck_expression(expr, scope, errors);
        }
    }
}


#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use crate::ast::expressions::Expression;
    use crate::scope::type_::TypeScope;
    use crate::ast::statement::Statement;
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ast::typing::statement::typecheck_statement;
    use crate::errors::Errors;

    #[test]
    fn expr() {
        // arrange
        let mut stmt = Statement::Expression(Expression::i32_literal(0, 1));
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_statement(&mut stmt, &scope, &mut errors);

        // assert
        assert_matches!(stmt, Statement::Expression(Expression {
            type_ref,
            ..
        }) => {
            assert_eq!(type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        });
    }
}