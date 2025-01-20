use crate::ast::expressions::binary::{BinaryExpression, BinaryOperator};
use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
use crate::errors::Errors;
use crate::scope::type_::TypeScope;

pub(super) fn typecheck_binary(
    expr: &mut BinaryExpression,
    scope: &TypeScope,
    errors: &mut Errors,
) -> TypeRef {
    typecheck_expression(expr.left.as_mut(), scope, errors);
    typecheck_expression(expr.right.as_mut(), scope, errors);

    let left_type = expr.left.type_ref.as_ref().unwrap();
    let right_type = expr.right.type_ref.as_ref().unwrap();

    if left_type == &TypeRef::Unknown || right_type == &TypeRef::Unknown || left_type != right_type {
        return TypeRef::Unknown;
    }

    match expr.op {
        BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div => {
            TypeRef::Builtin(BuiltinType::I32)
        }
        BinaryOperator::Equals
        | BinaryOperator::NotEquals
        | BinaryOperator::GreaterThan
        | BinaryOperator::LessThan
        | BinaryOperator::GreaterThanOrEquals
        | BinaryOperator::LessThanOrEquals => TypeRef::Builtin(BuiltinType::Bool),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use paste::paste;

    macro_rules! binary_tests {
        ($($left:tt $op:tt $right:tt -> $result:ident $(! $error:ident)? ;)*) => {
            $(
                binary_tests!(@error $left $op $right -> $result $(! $error)?);
            )*
        };

        (@error $left:tt $op:tt $right:tt -> $result:ident ! error) => {
            binary_tests!(@expand_left $left $op $right -> $result ! error );
        };
        (@error $left:tt $op:tt $right:tt -> $result:ident) => {
            binary_tests!(@expand_left $left $op $right -> $result ! noerror );
        };
        (@expand_left ($($left:ident),+) $op:tt $right:tt -> $result:ident ! $error:tt) => {
            $(
                binary_tests!(@expand_op $left $op $right -> $result ! $error);
            )+
        };
        (@expand_left $left:ident $op:tt $right:tt -> $result:ident ! $error:tt) => {
            binary_tests!(@expand_op $left $op $right -> $result ! $error);
        };
        (@expand_op $left:ident ($($op:tt),+) $right:tt -> $result:ident ! $error:tt) => {
            $(
                binary_tests!(@expand_right $left $op $right -> $result ! $error);
            )+
        };
        (@expand_op $left:ident $op:tt $right:tt -> $result:ident ! $error:tt) => {
            binary_tests!(@expand_right $left $op $right -> $result ! $error);
        };
        (@expand_right $left:ident $op:tt ($($right:ident),+) -> $result:ident ! $error:tt) => {
            $(
                binary_tests!(@op_name $left $op $right -> $result ! $error);
            )+
        };
        (@expand_right $left:ident $op:tt $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@op_name $left $op $right -> $result ! $error);
        };
        (@op_name $left:ident + $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, add, Add, $right, $result, $error);
        };
        (@op_name $left:ident - $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, sub, Sub, $right, $result, $error);
        };
        (@op_name $left:ident * $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, mul, Mul, $right, $result, $error);
        };
        (@op_name $left:ident / $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, div, Div, $right, $result, $error);
        };
        (@op_name $left:ident == $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, eq, Equals, $right, $result, $error);
        };
        (@op_name $left:ident != $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, ne, NotEquals, $right, $result, $error);
        };
        (@op_name $left:ident >= $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, gte, GreaterThanOrEquals, $right, $result, $error);
        };
        (@op_name $left:ident <= $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, lte, LessThanOrEquals, $right, $result, $error);
        };
        (@op_name $left:ident > $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, gt, GreaterThan, $right, $result, $error);
        };
        (@op_name $left:ident < $right:ident -> $result:ident ! $error:tt) => {
            binary_tests!(@func $left, lt, LessThan, $right, $result, $error);
        };

        (@literal i32) => { crate::ast::expressions::Expression::i32_literal(0, 1)};
        (@literal u32) => { crate::ast::expressions::Expression::u32_literal(0, 1)};
        (@literal f32) => { crate::ast::expressions::Expression::f32_literal(0, 1.0)};
        (@literal bool) => { crate::ast::expressions::Expression::bool_literal(0, true)};
        (@literal unknown) => { crate::ast::expressions::Expression::unknown()};

        (@typeref i32) => { crate::ast::typing::TypeRef::Builtin(crate::ast::typing::BuiltinType::I32) };
        (@typeref u32) => { crate::ast::typing::TypeRef::Builtin(crate::ast::typing::BuiltinType::U32) };
        (@typeref f32) => { crate::ast::typing::TypeRef::Builtin(crate::ast::typing::BuiltinType::F32) };
        (@typeref bool) => { crate::ast::typing::TypeRef::Builtin(crate::ast::typing::BuiltinType::Bool) };
        (@typeref unknown) => { crate::ast::typing::TypeRef::Unknown };

        (@func $left:ident, $opname:ident, $openum:ident, $right:ident, $result:ident, $error:ident) => {
            paste! {
                #[test]
                fn [< $opname _ $left _ $right >]() {
                    let op = crate::ast::expressions::binary::BinaryOperator :: $openum;
                    // arrange
                    let mut expr = crate::ast::expressions::Expression::binary(
                        0,
                        op,
                        binary_tests!(@literal $left),
                        binary_tests!(@literal $right),
                    );
                    let mut errors = crate::errors::Errors::new();
                    let scope = crate::scope::type_::TypeScope::new();

                    // act
                    crate::ast::typing::typecheck_expression(&mut expr, &scope, &mut errors);

                    // assert
                    assert_eq!(expr.type_ref, Some(binary_tests!(@typeref $result)));
                    binary_tests!(@assert-error $error, errors, op, $left, $opname, $right);
                }
            }
        };
        (@assert-error error, $errors:expr, $op:expr, $left:ident, $opname:ident, $right:ident) => {
            assert!($errors.has_error_at(0, crate::errors::ErrorKind :: BinaryOperationInvalidTypes(
                $op,
                binary_tests!(@typeref $left),
                binary_tests!(@typeref $right),
            )), "assertion failed: $errors.has_error_at(0, <errortype>({}, {}, {}))", stringify!($opname), stringify!($left), stringify!($right));
            assert_eq!($errors.get_errors().len(), 1);
        };
        (@assert-error noerror, $errors:expr, $op:expr, $left:ident, $opname:ident, $right:ident) => {
            assert!($errors.get_errors().is_empty());
        };
    }

    binary_tests! {
        i32 (+,-,*,/) i32 -> i32;
        u32 (+,-,*,/) u32 -> u32;
        f32 (+,-,*,/) f32 -> f32;

        i32 (>,>=,<,<=,==,!=) i32 -> bool;
        u32 (>,>=,<,<=,==,!=) u32 -> bool;
        f32 (>,>=,<,<=,==,!=) f32 -> bool;

        i32 (+,-,*,/,>,>=,<,<=,==,!=) (u32,f32,bool) -> unknown ! error;
        u32 (+,-,*,/,>,>=,<,<=,==,!=) (i32,f32,bool) -> unknown ! error;
        f32 (+,-,*,/,>,>=,<,<=,==,!=) (i32,u32,bool) -> unknown ! error;

        bool (+,-,*,/) (i32,u32,f32,bool) -> unknown ! error;
        bool (>,>=,<,<=,==,!=) (i32,u32,f32) -> unknown ! error;

        unknown (+,-,*,/,>,>=,<,<=,==,!=) (i32,u32,f32,bool,unknown) -> unknown;
        (i32,u32,f32,bool) (+,-,*,/,>,>=,<,<=,==,!=) unknown -> unknown;
    }

}
