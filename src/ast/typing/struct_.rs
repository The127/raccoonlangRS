use crate::ast::struct_decl::StructDecl;
use crate::ast::typing::map_type;
use crate::errors::Errors;
use crate::scope::type_::TypeScope;
use crate::types::type_ref::StructTypeMember;

pub fn typecheck_struct(decl: &mut StructDecl, scope: &TypeScope, errors: &mut Errors) {
    let members = &decl.members;
    let mut struct_type = decl.borrow_struct_type_mut();
    struct_type.members = members
        .iter()
        .map(|member| {
            let type_ref = map_type(&member.type_, errors, scope);
            StructTypeMember::new(member.name, type_ref)
        })
        .collect();
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::path::Path;
    use crate::ast::struct_decl::StructMember;
    use crate::ast::types::{NamedType, Type};
    use crate::ast::Visibility;
    use crate::errors::ErrorKind;
    use crate::types::type_ref::{BuiltinType, StructTypeMember, TypeRef};
    use assert_matches::assert_matches;
    use ustr::ustr;

    #[test]
    fn empty_struct() {
        // arrange
        let mut errors = Errors::new();
        let scope = TypeScope::new();

        let mut decl = StructDecl::new(0, Some(ustr("foo")), Visibility::Module, vec![]);

        // act
        typecheck_struct(&mut decl, &scope, &mut errors);

        // assert
        let type_ref = decl.get_typeref();
        assert_matches!(type_ref, TypeRef::Struct(s) => {
            let struct_type = s.borrow();
            assert_eq!(struct_type.members, vec![]);
        });

        errors.assert_empty();
    }

    #[test]
    fn nonempty_struct() {
        // arrange
        let mut errors = Errors::new();
        let scope = TypeScope::global();

        let mut decl = StructDecl::new(
            0,
            Some(ustr("Point")),
            Visibility::Module,
            vec![
                StructMember::new(
                    0,
                    ustr("x"),
                    Type::Named(NamedType::new(0, Path::name("i32"))),
                ),
                StructMember::new(
                    0,
                    ustr("y"),
                    Type::Named(NamedType::new(0, Path::name("i32"))),
                ),
            ],
        );

        // act
        typecheck_struct(&mut decl, &scope, &mut errors);

        // assert
        let type_ref = decl.get_typeref();
        assert_matches!(type_ref, TypeRef::Struct(s) => {
            let struct_type = s.borrow();
            assert_eq!(struct_type.members, vec![
                StructTypeMember::new(ustr("x"), TypeRef::Builtin(BuiltinType::I32)),
                StructTypeMember::new(ustr("y"), TypeRef::Builtin(BuiltinType::I32)),
            ]);
        });

        errors.assert_empty();
    }

    #[test]
    fn unknown_member_type() {
        // arrange
        let mut errors = Errors::new();
        let scope = TypeScope::global();

        let mut decl = StructDecl::new(
            0,
            Some(ustr("Point")),
            Visibility::Module,
            vec![
                StructMember::new(
                    1,
                    ustr("x"),
                    Type::Named(NamedType::new(1, Path::name("foo"))),
                ),
                StructMember::new(
                    2,
                    ustr("y"),
                    Type::Named(NamedType::new(2, Path::name("i32"))),
                ),
            ],
        );

        // act
        typecheck_struct(&mut decl, &scope, &mut errors);

        // assert
        let type_ref = decl.get_typeref();
        assert_matches!(type_ref, TypeRef::Struct(s) => {
            let struct_type = s.borrow();
            assert_eq!(struct_type.members, vec![
                StructTypeMember::new(ustr("x"), TypeRef::Unknown),
                StructTypeMember::new(ustr("y"), TypeRef::Builtin(BuiltinType::I32)),
            ]);
        });
        errors.assert_count(1);
        assert!(errors.has_error_at(1, ErrorKind::UnknownType(Path::name("foo"))))
    }
}
