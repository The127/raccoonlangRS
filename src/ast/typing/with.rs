use crate::ast::expressions::with::WithExpression;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::TypeRef;

pub(super) fn typecheck_with(
    expr: &mut WithExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    panic!("")
}

#[cfg(test)]
mod test {
    use ustr::ustr;
    use crate::ast::expressions::arg::Arg;
    use crate::ast::expressions::Expression;
    use crate::ast::path::Path;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::parser::ToSpanned;
    use crate::scope::type_::TypeScope;
    use crate::types::type_ref::TypeRef;

    #[test]
    fn target_without_values() {
        // arrange
        let mut expr = Expression::with(
            0,
            Expression::access(1, Path::name("a")),
            vec![
                Arg::shorthand(
                    2,
                    ustr("b").spanned(2),
                    Expression::access(2, Path::name("b"))
                ),
                Arg::named(
                    3,
                    ustr("c").spanned(3),
                    Expression::i32_literal(4, 1)
                ),
            ]);

        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(
            expr.type_ref(),
            Some(TypeRef::i32()),
        );
    }
}