use crate::ast::expressions::block::BlockExpression;
use crate::ast::scope::Scope;
use crate::ast::typing::statement::typecheck_statement;
use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
use crate::errors::Errors;

pub(super) fn typecheck_block(
    expr: &mut BlockExpression,
    scope: &dyn Scope,
    errors: &mut Errors,
) -> TypeRef {
    let mut has_decl = false;
    if let Some(let_) = expr.let_.as_mut() {
        if let Some(value) = let_.value.as_mut() {
            typecheck_expression(value, scope, errors);
            let_.type_ref = value.type_ref.clone();
        } else {
            let_.type_ref = Some(TypeRef::Unknown);
        }
        has_decl = true;

    }

    let new_scope = if has_decl {
        Some(scope.block(expr))
    } else {
        None
    };

    let scope = new_scope.as_ref().map(|x| x as &dyn Scope).unwrap_or(scope);

    for stmt in &mut expr.statements {
        typecheck_statement(stmt, scope, errors);
    }

    match expr.value.as_mut() {
        None => TypeRef::Builtin(BuiltinType::Unit),
        Some(val) => {
            typecheck_expression(val, scope, errors);
            val.type_ref.clone().unwrap()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::block::{BlockExpression, LetDeclaration};
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::scope::global::GlobalScope;
    use crate::ast::scope::MockScope;
    use crate::ast::statement::Statement;
    use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
    use crate::errors::Errors;
    use assert_matches::assert_matches;
    use ustr::ustr;

    #[test]
    fn empty_block() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));
    }

    #[test]
    fn i32_value() {
        // arrange
        let mut expr = Expression::block(0, vec![], Some(Expression::int_literal(0, 123)));
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn statements_no_value() {
        // arrange
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::unknown()),
                Statement::Expression(Expression::int_literal(0, 123)),
            ],
            None,
        );
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

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

    #[test]
    fn let_decl() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1))),
            vec![],
            None,
        );
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(expr.kind, ExpressionKind::Block(b) => {
            let decl = b.let_.unwrap();
            assert_eq!(decl.value.unwrap().type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
            assert_eq!(decl.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        });
    }

    #[test]
    fn let_decl_no_value() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), None),
            vec![],
            None,
        );
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(expr.kind, ExpressionKind::Block(b) => {
            let decl = b.let_.unwrap();
            assert_eq!(decl.type_ref, Some(TypeRef::Unknown));
        });
    }

    #[test]
    fn let_decl_with_access() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1))),
            vec![],
            Some(Expression::access(0, ustr("foo"))),
        );
        let mut errors = Errors::new();
        let scope = MockScope::new([]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
    }
}
