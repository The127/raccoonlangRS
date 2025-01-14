use crate::ast::function_decl::FunctionDecl;
use crate::scope::type_::TypeScope;
use crate::ast::types::{NamedType, Type};
use crate::ast::typing::{typecheck_expression, BuiltinType, TypeRef};
use crate::errors::Errors;
use ustr::ustr;

fn map_type(type_: &Type, scope: &TypeScope) -> TypeRef {
    match type_ {
        Type::Unknown => TypeRef::Unknown,
        Type::Unit => TypeRef::Builtin(BuiltinType::Unit),
        Type::Named(NamedType {
            path,
            ..
        }) if path.parts.len() == 1 => {
            let name = path.parts[0];
            if name == ustr("i32") {
                TypeRef::Builtin(BuiltinType::I32)
            } else if name == ustr("bool") {
                TypeRef::Builtin(BuiltinType::Bool)
            } else {
                todo!()
            }
        }
        _ => todo!(),
    }
}

pub fn typecheck_function(func: &mut FunctionDecl, scope: &TypeScope, errors: &mut Errors) {
    func.return_type.type_ref = Some(map_type(&func.return_type.type_, scope));
    for param in &mut func.parameters {
        param.type_ref = Some(map_type(&param.type_, scope));
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
    use crate::scope::type_::TypeScope;
    use crate::ast::types::Type;
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ast::Visibility;
    use parameterized::parameterized;
    use ustr::ustr;
    use crate::ast::path::Path;

    #[parameterized(params = {
        (Type::Unknown, TypeRef::Unknown),
        (Type::Unit, TypeRef::Builtin(BuiltinType::Unit)),
    })]
    fn map_type_unknown_or_unit(params: (Type, TypeRef)) {
        let (input, expected) = params;
        // arrange
        let type_ = input;
        let scope = TypeScope::new();

        // act
        let type_ref = map_type(&type_, &scope);

        // assert
        assert_eq!(type_ref, expected);
    }

    #[parameterized(params = {
        ("i32", TypeRef::Builtin(BuiltinType::I32)),
        ("bool", TypeRef::Builtin(BuiltinType::Bool)),
    })]
    fn map_type_named_builtin(params: (&str, TypeRef)) {
        let (name, expected) = params;
        // arrange
        let type_ = Type::Named(NamedType::new(0, Path::name(name)));
        let scope = TypeScope::new();

        // act
        let type_ref = map_type(&type_, &scope);

        // assert
        assert_eq!(type_ref, expected);
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
        let scope = TypeScope::new();
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
            decl.body.type_ref,
            Some(TypeRef::Builtin(BuiltinType::Unit))
        );
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
                    BinaryOperator::Equals,
                    Expression::access(0, Path::name("foo")),
                    Expression::access(0, Path::name("bar")),
                )),
            ),
        );
        let scope = TypeScope::new();
        let mut errors = Errors::new();


        // act
        typecheck_function(&mut decl, &scope, &mut errors);

        // assert
        assert_eq!(
            decl.body.type_ref,
            Some(TypeRef::Builtin(BuiltinType::Bool))
        );

    }
}
