use crate::add_error;
use crate::ast::expressions::arg::Arg;
use crate::ast::expressions::new::NewExpression;
use crate::ast::typing::typecheck_expression;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::source_map::HasSpan;
use crate::types::type_ref::TypeRef;

pub(super) fn typecheck_new(
    expr: &mut NewExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    let (result, struct_type) = match scope.lookup(&expr.path) {
        None => {
            add_error!(errors, expr.span(), UnknownType(expr.path.clone()));
            (TypeRef::Unknown, None)
        },
        Some(x) => {
            match x {
                TypeRef::Struct(s) => {
                    (x.clone(), Some(s))
                },
                _ => {
                    add_error!(errors, expr.span(), NewOnNonStruct(expr.path.clone()));
                    (TypeRef::Unknown, None)
                },
            }
        },
    };

    for arg in &mut expr.args {
        match arg{
            Arg::Named(named) => {
                typecheck_expression(&mut named.value, scope, errors);
                if let Some(s) = struct_type {
                    let member_exists = s.borrow().members.iter().any(|m| m.name == named.name.value);
                    if member_exists == false {
                        add_error!(errors, arg.span(), UnexpectedArgument);
                    }
                }
            },
            Arg::Unnamed(_) => {
                panic!("There should no longer be any unnamed args at this point, they should have been transformed into errors or shorthands.")
            },
        }
    }

    result
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use ustr::ustr;
    use crate::ast::expressions::arg::{Arg, NamedArg};
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::expressions::new::NewExpression;
    use crate::ast::path::Path;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::ToSpanned;
    use crate::refs::MutableRef;
    use crate::scope::type_::TypeScope;
    use crate::types::type_ref::{BuiltinType, StructType, StructTypeMember, TypeRef};

    #[test]
    fn type_does_not_exist() {
        // arrange
        let scope = TypeScope::global();
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Unknown));
        assert!(errors.has_error_at(
            0,
            ErrorKind::UnknownType(Path::name("foo")),
        ));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn new_no_args() {
        // arrange
        let struct_type_ref = MutableRef::<StructType>::default();
        {
            let mut struct_type = struct_type_ref.borrow_mut();
            struct_type.members = vec![];
        }
        let type_ref = TypeRef::Struct(struct_type_ref.get_immutable());

        let scope = TypeScope::from(&[(ustr("foo"), type_ref.clone())]);
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(type_ref));
        errors.assert_empty();
    }

    #[test]
    fn type_is_not_a_struct() {
        // arrange
        let scope = TypeScope::from(&[(ustr("foo"), TypeRef::bool())]);
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Unknown));
        assert!(errors.has_error_at(
            0,
            ErrorKind::NewOnNonStruct(Path::name("foo")),
        ));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    #[should_panic]
    fn new_unnamed_args() {
        // arrange
        let struct_type_ref = MutableRef::<StructType>::default();
        {
            let mut struct_type = struct_type_ref.borrow_mut();
            struct_type.members = vec![];
        }
        let type_ref = TypeRef::Struct(struct_type_ref.get_immutable());

        let scope = TypeScope::from(&[(ustr("foo"), type_ref.clone())]);
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![
            Arg::unnamed(Expression::bool_literal(0, true)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);
    }

    #[test]
    fn new_named_args() {
        // arrange
        let struct_type_ref = MutableRef::<StructType>::default();
        {
            let mut struct_type = struct_type_ref.borrow_mut();
            struct_type.members = vec![StructTypeMember::new(
                ustr("bar"),
                TypeRef::Builtin(BuiltinType::I32),
            )];
        }
        let type_ref = TypeRef::Struct(struct_type_ref.get_immutable());

        let scope = TypeScope::from(&[(ustr("foo"), type_ref.clone())]);
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![
            Arg::named(0, ustr("bar").spanned(0), Expression::bool_literal(0, true)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(&expr, Expression {
           kind: ExpressionKind::New(
                NewExpression  {
                    args,
                    ..
                },
            ),
            ..
        } => {
            assert_matches!(&args[0], Arg::Named(
                NamedArg{
                    value,
                    ..
                },
            ) => {
                assert_eq!(value.type_ref(), Some(TypeRef::bool()));
            })
        });
        assert_eq!(expr.type_ref(), Some(type_ref));
        errors.assert_empty();
    }

    #[test]
    fn type_does_not_exist_with_args() {
        // arrange
        let scope = TypeScope::global();
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![
            Arg::named(0, ustr("bar").spanned(0), Expression::bool_literal(0, true)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_matches!(&expr, Expression {
           kind: ExpressionKind::New(
                NewExpression  {
                    args,
                    ..
                },
            ),
            ..
        } => {
            assert_matches!(&args[0], Arg::Named(
                NamedArg{
                    value,
                    ..
                },
            ) => {
                assert_eq!(value.type_ref(), Some(TypeRef::bool()));
            })
        });
        assert_eq!(expr.type_ref(), Some(TypeRef::Unknown));
        assert!(errors.has_error_at(
            0,
            ErrorKind::UnknownType(Path::name("foo")),
        ));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn unexpected_argument() {
        // arrange
        let struct_type_ref = MutableRef::<StructType>::default();
        {
            let mut struct_type = struct_type_ref.borrow_mut();
            struct_type.members = vec![];
        }
        let type_ref = TypeRef::Struct(struct_type_ref.get_immutable());

        let scope = TypeScope::from(&[(ustr("foo"), type_ref.clone())]);
        let mut errors = Errors::new();

        let mut expr = Expression::new_(0, Path::name("foo"), vec![
            Arg::named(0, ustr("bar").spanned(0), Expression::bool_literal(0, true)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(type_ref));
        assert!(errors.has_error_at(
            0,
            ErrorKind::UnexpectedArgument,
        ));
        assert_eq!(errors.get_errors().len(), 1);
    }
}