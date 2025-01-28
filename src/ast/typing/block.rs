use crate::ast::expressions::block::BlockExpression;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::typing::statement::typecheck_statement;
use crate::ast::typing::{typecheck_expression};
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::{BuiltinType, TypeRef};

pub(super) fn typecheck_block(
    expr: &mut BlockExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    let mut has_decl = false;
    if let Some(let_) = expr.let_.as_mut() {
        typecheck_expression(&mut let_.value, scope, errors);
        let_.type_ref = Some(let_.value.get_type(TypeCoercionHint::NoCoercion, errors));

        has_decl = true;
    }

    let new_scope = if has_decl {
        Some(scope.block(expr))
    } else {
        None
    };

    let scope = new_scope.as_ref().map(|x| x as &TypeScope).unwrap_or(scope);

    for stmt in &mut expr.statements {
        typecheck_statement(stmt, scope, errors);
    }

    match expr.value.as_mut() {
        None => TypeRef::Builtin(BuiltinType::Unit),
        Some(val) => {
            typecheck_expression(val, scope, errors);
            val.get_type(TypeCoercionHint::NoCoercion, errors)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::path::Path;
    use crate::ast::pattern::Pattern;
    use crate::ast::statement::Statement;
    use crate::ast::typing::typecheck_expression;
    use crate::types::type_ref::{BuiltinType, TupleType, TypeRef};
    use crate::errors::Errors;
    use crate::scope::type_::TypeScope;
    use assert_matches::assert_matches;
    use ustr::ustr;
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::expressions::block::{BlockExpression, LetDeclaration};

    #[test]
    fn empty_block() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::Unit)));
    }

    #[test]
    fn i32_value() {
        // arrange
        let mut expr = Expression::block(0, vec![], Some(Expression::i32_literal(0, 123)));
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn statements_no_value() {
        // arrange
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::unknown()),
                Statement::Expression(Expression::i32_literal(0, 123)),
            ],
            None,
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::Unit)));
        assert_matches!(expr.kind, ExpressionKind::Block(BlockExpression {
            statements,
            ..
        }) => {
            assert_matches!(&statements[..], [
                Statement::Expression(s1),
                Statement::Expression(s2),
            ] => {
                assert_eq!(s1.type_ref(), Some(TypeRef::Unknown));
                assert_eq!(s2.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
            })
        });
    }

    #[test]
    fn let_decl() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(
                0,
                Pattern::Name(ustr("foo")),
                Expression::i32_literal(0, 1),
            ),
            vec![],
            None,
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(expr.kind, ExpressionKind::Block(b) => {
            let decl = b.let_.unwrap();
            assert_eq!(decl.value.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
            assert_eq!(decl.type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        });
    }

    #[test]
    fn let_decl_no_value() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, Pattern::Name(ustr("foo")), Expression::unknown()),
            vec![],
            None,
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

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
            LetDeclaration::new(
                0,
                Pattern::Name(ustr("foo")),
                Expression::i32_literal(0, 1),
            ),
            vec![],
            Some(Expression::access(0, Path::name("foo"))),
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Builtin(BuiltinType::I32)));
    }

    #[test]
    fn let_decl_with_discard() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, Pattern::Discard, Expression::i32_literal(0, 1)),
            vec![],
            None,
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(expr.kind, ExpressionKind::Block(block) => {
            assert_eq!(block.let_.unwrap().type_ref, Some(TypeRef::Builtin(BuiltinType::I32)));
        });
    }

    #[test]
    fn let_decl_with_tuple() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(
                0,
                Pattern::Tuple(vec![
                    Pattern::Name(ustr("foo")),
                    Pattern::Tuple(vec![
                        Pattern::Name(ustr("x")),
                        Pattern::Name(ustr("y")),
                        Pattern::Discard,
                    ]),
                ]),
                Expression::tuple(
                    0,
                    vec![
                        Expression::bool_literal(0, true),
                        Expression::tuple(
                            0,
                            vec![
                                Expression::i32_literal(0, 1),
                                Expression::i32_literal(0, 2),
                                Expression::i32_literal(0, 3),
                            ],
                        ),
                    ],
                ),
            ),
            vec![],
            Some(Expression::tuple(0, vec![
                Expression::access(0, Path::name("foo")),
                Expression::access(0, Path::name("x")),
                Expression::access(0, Path::name("y")),
            ])),
        );
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Tuple(TupleType {
            fields: vec![
                TypeRef::Builtin(BuiltinType::Bool),
                TypeRef::Builtin(BuiltinType::I32),
                TypeRef::Builtin(BuiltinType::I32),
            ],
        })));
    }
}
