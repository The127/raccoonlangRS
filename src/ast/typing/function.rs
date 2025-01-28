use crate::add_error;
use crate::ast::expressions::TypeCoercionHint;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::types::{NamedType, Type};
use crate::ast::typing::{map_type, typecheck_expression};
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::source_map::HasSpan;
use crate::types::type_ref::{BuiltinType, FunctionTypeParam, TypeRef};

pub fn typecheck_function(decl: &mut FunctionDecl, scope: &TypeScope, errors: &mut Errors) {
    let return_type = map_type(&decl.return_type.type_, errors, scope);

    decl.return_type.type_ref = Some(return_type.clone());

    let mut param_types = vec![];
    for param in &mut decl.parameters {
        let param_type = map_type(&param.type_, errors, scope);
        param_types.push(FunctionTypeParam::new(Some(param.name), param_type.clone()));
        param.type_ref = Some(param_type);
    }
    let mut function_type = decl.borrow_function_type_mut();

    function_type.return_ = return_type;
    function_type.params = param_types;
}

pub fn typecheck_function_interior(
    decl: &mut FunctionDecl,
    scope: &TypeScope,
    errors: &mut Errors,
) {
    let func_scope = scope.function(&decl);

    typecheck_expression(&mut decl.body, &func_scope, errors);

    let expected_return = decl.return_type.type_ref.clone().unwrap();
    decl.body.get_type(TypeCoercionHint::Specific(expected_return), errors);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::Expression;
    use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
    use crate::ast::path::Path;
    use crate::ast::types::Type;
    use crate::ast::Visibility;
    use crate::errors::ErrorKind;
    use crate::parser::ToSpanned;
    use crate::scope::type_::TypeScope;
    use crate::types::type_ref::{BuiltinType, TypeRef};
    use assert_matches::assert_matches;
    use ustr::ustr;

    #[test]
    fn params_and_return() {
        // arrange
        let mut decl = FunctionDecl::new(
            0,
            Some(ustr("foobar")),
            Visibility::Module,
            vec![
                FunctionParameter::new(
                    0,
                    ustr("foo"),
                    Type::Named(NamedType::new(0, Path::name("i32"))),
                ),
                FunctionParameter::new(
                    0,
                    ustr("bar"),
                    Type::Named(NamedType::new(0, Path::name("bool"))),
                ),
            ],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: None,
            },
            Expression::block(0, vec![], None),
        );
        let scope = TypeScope::global();
        let mut errors = Errors::new();

        // act
        typecheck_function(&mut decl, &scope, &mut errors);

        // assert
        assert_eq!(
            decl.return_type.type_ref,
            Some(TypeRef::Builtin(BuiltinType::Unit))
        );
        assert_eq!(
            decl.parameters
                .iter()
                .map(|x| x.type_ref.clone())
                .collect::<Vec<_>>(),
            vec![
                Some(TypeRef::Builtin(BuiltinType::I32)),
                Some(TypeRef::Builtin(BuiltinType::Bool)),
            ]
        );

        assert_matches!(decl.get_typeref(), TypeRef::Function(f) => {
            let func_type = f.borrow();
            assert_eq!(func_type.return_, TypeRef::Builtin(BuiltinType::Unit));
            assert_eq!(func_type.params, vec![
                FunctionTypeParam::new(Some(ustr("foo")), TypeRef::Builtin(BuiltinType::I32)),
                FunctionTypeParam::new(Some(ustr("bar")), TypeRef::Builtin(BuiltinType::Bool)),
            ]);
        });
        errors.assert_empty();
    }

    #[test]
    fn function_params_are_in_scope_for_body() {
        // arrange
        let mut decl = FunctionDecl::new(
            0,
            Some(ustr("foobar")),
            Visibility::Module,
            vec![
                FunctionParameter::new(
                    0,
                    ustr("foo"),
                    Type::Named(NamedType::new(0, Path::name("i32"))),
                ),
                FunctionParameter::new(
                    0,
                    ustr("bar"),
                    Type::Named(NamedType::new(0, Path::name("i32"))),
                ),
            ],
            FunctionReturnType {
                type_: Type::Named(NamedType::new(0, Path::name("bool"))),
                type_ref: None,
            },
            Expression::block(
                0,
                vec![],
                Some(Expression::binary(
                    0,
                    BinaryOperator::Equals.spanned_empty(),
                    Expression::access(0, Path::name("foo")),
                    Expression::access(0, Path::name("bar")),
                )),
            ),
        );

        let scope = TypeScope::global();
        let mut errors = Errors::new();
        typecheck_function(&mut decl, &scope, &mut errors);

        // act
        typecheck_function_interior(&mut decl, &scope, &mut errors);

        // assert
        assert_eq!(
            decl.body.type_ref(),
            Some(TypeRef::Builtin(BuiltinType::Bool))
        );
        errors.assert_empty();
    }

    #[test]
    fn unknown_param_and_return_types_are_error() {
        // arrange
        let mut decl = FunctionDecl::new(
            0,
            None,
            Visibility::Module,
            vec![
                FunctionParameter::new(
                    0,
                    ustr("foo"),
                    Type::Named(NamedType::new(1..5, Path::name("asdf"))),
                ),
                FunctionParameter::new(
                    0,
                    ustr("bar"),
                    Type::Named(NamedType::new(6..10, Path::name("xyz"))),
                ),
            ],
            FunctionReturnType {
                type_: Type::Named(NamedType::new(20..23, Path::name("ab"))),
                type_ref: None,
            },
            Expression::block(0, vec![], None),
        );
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        typecheck_function(&mut decl, &scope, &mut errors);

        // assert
        assert_eq!(decl.parameters[0].type_ref, Some(TypeRef::Unknown));
        assert_eq!(decl.parameters[1].type_ref, Some(TypeRef::Unknown));
        assert!(errors.has_error_at(1..5, ErrorKind::UnknownType(Path::name("asdf"))));
        assert!(errors.has_error_at(6..10, ErrorKind::UnknownType(Path::name("xyz"))));
        assert!(errors.has_error_at(20..23, ErrorKind::UnknownType(Path::name("ab"))));
        assert_eq!(errors.get_errors().len(), 3);
    }

    #[test]
    fn body_doesnt_match_declared_return_type() {
        // arrange
        let mut decl = FunctionDecl::new(
            0,
            Some(ustr("foobar")),
            Visibility::Module,
            vec![],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: None,
            },
            Expression::block(0, vec![], Some(Expression::i32_literal(7, 123))),
        );
        let scope = TypeScope::global();
        let mut errors = Errors::new();
        typecheck_function(&mut decl, &scope, &mut errors);

        // act
        typecheck_function_interior(&mut decl, &scope, &mut errors);

        // assert
        errors.assert_count(1);
        assert!(errors.has_error_at(
            7,
            ErrorKind::TypeMismatch(
                TypeRef::Builtin(BuiltinType::I32),
                TypeRef::Builtin(BuiltinType::Unit)
            )
        ));
    }

    #[test]
    fn body_doesnt_match_declared_return_type_multiple_branches() {
        // arrange
        let mut decl = FunctionDecl::new(
            0,
            Some(ustr("foobar")),
            Visibility::Module,
            vec![],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: None,
            },
            Expression::block(0, vec![], Some(Expression::if_(
                0,
                Expression::bool_literal(0, true),
                Expression::block(0, vec![], Some(Expression::i32_literal(7, 123))),
                Some(Expression::block(0, vec![], Some(Expression::f32_literal(17, 1.23))))
            ))),
        );
        let scope = TypeScope::global();
        let mut errors = Errors::new();
        typecheck_function(&mut decl, &scope, &mut errors);

        // act
        typecheck_function_interior(&mut decl, &scope, &mut errors);

        // assert
        errors.assert_count(2);
        assert!(errors.has_error_at(
            7,
            ErrorKind::TypeMismatch(
                TypeRef::Builtin(BuiltinType::I32),
                TypeRef::Builtin(BuiltinType::Unit)
            )
        ));
        assert!(errors.has_error_at(
            17,
            ErrorKind::TypeMismatch(
                TypeRef::Builtin(BuiltinType::F32),
                TypeRef::Builtin(BuiltinType::Unit)
            )
        ));
    }
}
