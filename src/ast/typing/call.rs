use crate::add_error;
use crate::ast::expressions::call::CallExpression;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::typing::typecheck_expression;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::source_map::HasSpan;
use crate::types::type_ref::TypeRef;

pub(super) fn typecheck_call(
    expr: &mut CallExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    typecheck_expression(&mut expr.target, scope, errors);
    let target_type = expr.target.get_type(TypeCoercionHint::Any, errors);

    match target_type {
        TypeRef::Function(x) => {
            let function_type = x.borrow();
            if function_type.params.len() > expr.args.len() {
                add_error!(errors, expr.span().end()-1, MissingArgument(function_type.params.last().unwrap().name.unwrap()));
            }

            // let found_params = function_type.params.iter().map(|x| false).collect::<Vec<_>>();


            function_type.return_.clone()
        }
        TypeRef::Unknown => TypeRef::Unknown,
        _ => todo!()
    }
}


#[cfg(test)]
mod test {
    use crate::ast::expressions::arg::Arg;
    use crate::ast::expressions::Expression;
    use crate::ast::path::Path;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::ToSpanned;
    use crate::refs::MutableRef;
    use crate::scope::type_::TypeScope;
    use crate::types::type_ref::{FunctionType, FunctionTypeParam, TypeRef};
    use ustr::ustr;
    use crate::source_map::Span;

    #[test]
    fn fn_call_positional_only() {
        // arrange
        let func_type_ref = MutableRef::<FunctionType>::default();
        {
            let mut func_type = func_type_ref.borrow_mut();
            func_type.return_ = TypeRef::i32();
            func_type.params = vec![FunctionTypeParam::new(None, TypeRef::i32()), FunctionTypeParam::new(None, TypeRef::i32())];
        }
        let type_ref = TypeRef::Function(func_type_ref.get_immutable());

        let mut scope = TypeScope::global();
        scope.insert(ustr("foo"), type_ref);
        let mut errors = Errors::new();

        let mut expr = Expression::call(0, Expression::access(0, Path::name("foo")), vec![
            Arg::unnamed(Expression::i32_literal(0, 1)),
            Arg::unnamed(Expression::i32_literal(0, 2)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_empty();
    }

    #[test]
    fn fn_call_positional_and_named() {
        // arrange
        let func_type_ref = MutableRef::<FunctionType>::default();
        {
            let mut func_type = func_type_ref.borrow_mut();
            func_type.return_ = TypeRef::i32();
            func_type.params = vec![FunctionTypeParam::new(Some(ustr("a")), TypeRef::i32()), FunctionTypeParam::new(Some(ustr("b")), TypeRef::i32())];
        }
        let type_ref = TypeRef::Function(func_type_ref.get_immutable());
        let mut scope = TypeScope::global();
        scope.insert(ustr("foo"), type_ref);
        let mut errors = Errors::new();

        let mut expr = Expression::call(0, Expression::access(0, Path::name("foo")), vec![
            Arg::unnamed(Expression::i32_literal(0, 1)),
            Arg::named(0, ustr("b").spanned_empty(), Expression::i32_literal(0, 2)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_empty();
    }

    #[test]
    fn call_unknown() {
        // arrange
        let scope = TypeScope::global();
        let mut errors = Errors::new();

        let mut expr = Expression::call(0, Expression::access(5, Path::name("foo")), vec![
            Arg::unnamed(Expression::i32_literal(0, 1)),
            Arg::named(0, ustr("b").spanned_empty(), Expression::i32_literal(0, 2)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::Unknown));
        errors.has_error_at(5, ErrorKind::UnknownVariable(Path::name("foo")));
        errors.assert_count(1);
    }

    fn scope_with_foo_named_args() -> TypeScope<'static> {
        let func_type_ref = MutableRef::<FunctionType>::default();
        {
            let mut func_type = func_type_ref.borrow_mut();
            func_type.return_ = TypeRef::i32();
            func_type.params = vec![FunctionTypeParam::new(Some(ustr("a")), TypeRef::i32()), FunctionTypeParam::new(Some(ustr("b")), TypeRef::i32())];
        }
        let type_ref = TypeRef::Function(func_type_ref.get_immutable());
        let mut scope = TypeScope::global();
        scope.insert(ustr("foo"), type_ref);
        scope
    }

    #[test]
    fn call_missing_multiple_args() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(2);
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("a"))));
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("b"))));
    }

    #[test]
    fn call_positional_arg_missing_second() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![
            Arg::unnamed(Expression::i32_literal(0, 1))
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("b"))));
    }

    #[test]
    fn call_named_arg_missing_second() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![
            Arg::named(0, ustr("a").spanned_empty(), Expression::i32_literal(0, 1))
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("b"))));
    }

    #[test]
    fn call_named_arg_missing_first() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![
            Arg::named(0, ustr("b").spanned_empty(), Expression::i32_literal(0, 1))
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("a"))));
    }

    #[test]
    fn call_positional_after_named() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![
            Arg::named(0, ustr("a").spanned_empty(), Expression::i32_literal(0, 1)),
            Arg::unnamed(Expression::i32_literal(3, 2))
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(2, ErrorKind::PositionalArgumentAfterNamed));
    }

    #[test]
    fn call_positional_too_many() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![
            Arg::unnamed(Expression::i32_literal(2, 1)),
            Arg::unnamed(Expression::i32_literal(3, 2)),
            Arg::unnamed(Expression::i32_literal(4, 3)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(4, ErrorKind::UnexpectedArgument));
    }

    #[test]
    fn call_named_with_extra_positional() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..10, Expression::access(0, Path::name("foo")), vec![
            Arg::named(2, ustr("a").spanned_empty(), Expression::i32_literal(2, 1)),
            Arg::named(3, ustr("b").spanned_empty(), Expression::i32_literal(3, 2)),
            Arg::unnamed(Expression::i32_literal(4, 3)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(4, ErrorKind::UnexpectedArgument));
    }

    #[test]
    fn call_named_with_extra_named() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..12, Expression::access(0, Path::name("foo")), vec![
            Arg::named(2..4, ustr("a").spanned_empty(), Expression::i32_literal(3, 1)),
            Arg::named(5..7, ustr("b").spanned_empty(), Expression::i32_literal(6, 2)),
            Arg::named(8..10, ustr("c").spanned_empty(), Expression::i32_literal(9, 3)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(9, ErrorKind::UnexpectedArgument));
    }

    #[test]
    fn call_missing_and_extra_named() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..12, Expression::access(0, Path::name("foo")), vec![
            Arg::named(2..4, ustr("x").spanned_empty(), Expression::i32_literal(3, 1)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(3);
        assert!(errors.has_error_at(3, ErrorKind::UnexpectedArgument));
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("a"))));
        assert!(errors.has_error_at(9, ErrorKind::MissingArgument(ustr("b"))));
    }

    #[test]
    fn call_positional_and_named_duplicate() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..12, Expression::access(0, Path::name("foo")), vec![
            Arg::unnamed(Expression::i32_literal(1, 1)),
            Arg::unnamed(Expression::i32_literal(2, 2)),
            Arg::named(2..4, ustr("a").spanned_empty(), Expression::i32_literal(3, 3)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(3, ErrorKind::DuplicateArgument(1.into())));
    }

    #[test]
    fn call_named_duplicate() {
        // arrange
        let scope = scope_with_foo_named_args();
        let mut errors = Errors::new();

        let mut expr = Expression::call(1..12, Expression::access(0, Path::name("foo")), vec![
            Arg::named(2..4, ustr("a").spanned(2), Expression::i32_literal(3, 3)),
            Arg::named(5..7, ustr("b").spanned(5), Expression::i32_literal(6, 3)),
            Arg::named(8..10, ustr("a").spanned(8), Expression::i32_literal(9, 3)),
        ]);

        // act
        typecheck_expression(&mut expr, &scope, &mut errors);

        // assert
        assert_eq!(expr.type_ref(), Some(TypeRef::i32()));
        errors.assert_count(1);
        assert!(errors.has_error_at(8..10, ErrorKind::DuplicateArgument(Span(2, 4))));
    }

    // further wrong argument situations:
    //
    // given foo(a: i32, b: i32 = 123)
    // foo() => missing argument "a"
    // foo(b=1) => missing argument "a"
    // foo(a=1, 2) => positional argument after named argument
    // foo(1,2,3) => unexpected argument
    // foo(x=1) => missing argument "a", unexpected argument "x"
    // foo(1,2,a=3) => argument "a" already provided
    // foo(1,a=3) => argument "a" already provided
    // foo(a=1, a=2, b=3) => argument "a" already provided
    //
    // given foo(i32,i32)
    // foo() => missing argument 1, missing argument 2
    // foo(1) => missing argument 2
    // foo(1,2,3) => unexpected argument
    // foo(a=1) => missing argument 1, missing argument 2, unexpected argument "a"
}