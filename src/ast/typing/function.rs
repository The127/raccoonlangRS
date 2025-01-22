use crate::ast::function_decl::FunctionDecl;
use crate::ast::types::{NamedType, Type};
use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
use crate::errors::{ErrorKind, Errors};
use crate::scope::type_::TypeScope;
use ustr::ustr;
use crate::add_error;
use crate::source_map::HasSpan;

fn map_type(type_: &Type, errors: &mut Errors, scope: &TypeScope) -> TypeRef {
    match type_ {
        Type::Unknown => TypeRef::Unknown,
        Type::Unit => TypeRef::Builtin(BuiltinType::Unit),
        Type::Named(named_type) => {
            let type_ref = scope.lookup(&named_type.path);
            if let Some(t) = type_ref {
                t.clone()
            } else {
                add_error!(errors, named_type.span(), UnknownType(named_type.path.clone()));
                TypeRef::Unknown
            }
        }
    }
}

pub fn typecheck_function(func: &mut FunctionDecl, scope: &TypeScope, errors: &mut Errors) {
    // let ret_type = map_type(&func.return_type.type_, scope);

    func.return_type.type_ref = Some(map_type(&func.return_type.type_, errors, scope));
    for param in &mut func.parameters {
        param.type_ref = Some(map_type(&param.type_, errors, scope));
    }

    let func_scope = scope.function(&func);

    typecheck_expression(&mut func.body, &func_scope, errors);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::Expression;
    use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
    use crate::ast::path::Path;
    use crate::ast::types::Type;
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ast::Visibility;
    use crate::parser::ToSpanned;
    use crate::scope::type_::TypeScope;
    use parameterized::parameterized;
    use ustr::ustr;
    use crate::errors::ErrorKind;

    #[test]
    fn map_unit() {
        // arrange
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&Type::Unit, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Builtin(BuiltinType::Unit));
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn map_unknown() {
        // arrange
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&Type::Unknown, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Unknown);
    }

    #[parameterized(params = {
        ("i32", TypeRef::Builtin(BuiltinType::I32)),
        ("u32", TypeRef::Builtin(BuiltinType::U32)),
        ("f32", TypeRef::Builtin(BuiltinType::F32)),
        ("bool", TypeRef::Builtin(BuiltinType::Bool)),
    })]
    fn map_type_named_builtin(params: (&str, TypeRef)) {
        let (name, expected) = params;
        // arrange
        let type_ = Type::Named(NamedType::new(0, Path::name(name)));
        let scope = TypeScope::global();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&type_, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, expected);
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn map_unknown_named_type() {
        // arrange
        let type_ = Type::Named(NamedType::new(7..10, Path::name("foo")));
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        let type_ref = map_type(&type_, &mut errors, &scope);

        // assert
        assert_eq!(type_ref, TypeRef::Unknown);
        assert!(errors.has_error_at(7..10, ErrorKind::UnknownType(Path::name("foo"))));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn simple_function() {
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
        assert_eq!(
            decl.body.type_ref(),
            Some(TypeRef::Builtin(BuiltinType::Unit))
        );
        assert!(errors.get_errors().is_empty());
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
                type_: Type::Unit,
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

        // act
        typecheck_function(&mut decl, &scope, &mut errors);

        // assert
        assert_eq!(
            decl.body.type_ref(),
            Some(TypeRef::Builtin(BuiltinType::Bool))
        );
        assert!(errors.get_errors().is_empty());
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
            Expression::block(
                0,
                vec![],
                None,
            ),
        );
        let scope = TypeScope::new();
        let mut errors = Errors::new();

        // act
        typecheck_function(&mut decl, &scope, &mut errors);

        // assert
        assert_eq!(
            decl.body.type_ref(),
            Some(TypeRef::Builtin(BuiltinType::Unit))
        );
        assert_eq!(decl.parameters[0].type_ref, Some(TypeRef::Unknown));
        assert_eq!(decl.parameters[1].type_ref, Some(TypeRef::Unknown));
        assert!(errors.has_error_at(1..5, ErrorKind::UnknownType(Path::name("asdf"))));
        assert!(errors.has_error_at(6..10, ErrorKind::UnknownType(Path::name("xyz"))));
        assert!(errors.has_error_at(20..23, ErrorKind::UnknownType(Path::name("ab"))));
        assert_eq!(errors.get_errors().len(), 3);
    }
}
