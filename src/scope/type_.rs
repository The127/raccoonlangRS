use crate::ast::expressions::block::BlockExpression;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::pattern::Pattern;
use crate::ast::typing::{BuiltinType, TupleType, TypeRef};
use crate::scope::Scope;
use ustr::{ustr, UstrMap};

pub type TypeScope<'a> = Scope<'a, TypeRef>;

fn extract_pattern_types(typemap: &mut UstrMap<TypeRef>, pattern: &Pattern, type_ref: &TypeRef) {
    match pattern {
        Pattern::Discard => (),
        Pattern::Name(name) => {
            typemap.insert(*name, type_ref.clone());
        }
        Pattern::Tuple(pattern_fields) => {
            match type_ref {
                TypeRef::Tuple(TupleType {fields: type_fields}) => {
                    if pattern_fields.len() != type_fields.len() {
                        todo!("error handling")
                    }
                    for (pattern, type_ref) in pattern_fields.iter().zip(type_fields.iter()) {
                        extract_pattern_types(typemap, pattern, type_ref);
                    }
                },
                _ => todo!("error handling"),
            }
        }
    }
}

impl TypeScope<'_> {
    pub fn global() -> Self {
        Self::from(&[
            (ustr("i32"), TypeRef::Builtin(BuiltinType::I32)),
            (ustr("u32"), TypeRef::Builtin(BuiltinType::U32)),
            (ustr("f32"), TypeRef::Builtin(BuiltinType::F32)),
            (ustr("bool"), TypeRef::Builtin(BuiltinType::Bool)),
            (ustr("unit"), TypeRef::Builtin(BuiltinType::Unit)),
        ])
    }

    pub fn function(&self, func: &FunctionDecl) -> TypeScope {
        let mut values = UstrMap::default();

        for param in &func.parameters {
            values.insert(param.name, param.type_ref.clone().unwrap());
        }
        TypeScope {
            parent: Some(self),
            values: values,
        }
    }

    pub fn block(&self, block: &BlockExpression) -> TypeScope {
        let decl = block
            .let_
            .as_ref()
            .expect("only create BlockScope for blocks that have a decl");

        let mut values = UstrMap::default();

        let type_ref = decl
            .type_ref
            .as_ref()
            .expect("decl must already have been typechecked");

        extract_pattern_types(&mut values, &decl.binding, type_ref);

        TypeScope {
            parent: Some(self),
            values: values,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::function_decl::{FunctionParameter, FunctionReturnType};
    use crate::ast::path::Path;
    use crate::ast::types::{NamedType, Type};
    use crate::ast::typing::BuiltinType;
    use crate::ast::Visibility;
    use assert_matches::assert_matches;
    use ustr::ustr;

    #[test]
    fn function() {
        // arrange
        let global = TypeScope::global();
        let func_decl = FunctionDecl::new(
            0,
            None,
            Visibility::Module,
            vec![
                FunctionParameter::new(
                    0,
                    ustr("foo"),
                    Type::Named(NamedType::new(0, Path::name("i32"))),
                )
                .with_type_ref(TypeRef::Builtin(BuiltinType::I32)),
                FunctionParameter::new(
                    0,
                    ustr("bar"),
                    Type::Named(NamedType::new(0, Path::name("bool"))),
                )
                .with_type_ref(TypeRef::Builtin(BuiltinType::Bool)),
            ],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: Some(TypeRef::Builtin(BuiltinType::Unit)),
            },
            Expression::block(0, vec![], None),
        );

        // act
        let func_scope = global.function(&func_decl);

        // assert
        assert_eq!(func_scope.values.len(), 2);
        assert_eq!(
            func_scope.values.get(&ustr("foo")),
            Some(&TypeRef::Builtin(BuiltinType::I32))
        );
        assert_eq!(
            func_scope.values.get(&ustr("bar")),
            Some(&TypeRef::Builtin(BuiltinType::Bool))
        );
    }

    #[test]
    fn block_name_pattern() {
        // arrange
        let global = TypeScope::global();
        let expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(
                0,
                Pattern::Name(ustr("foo")),
                Expression::i32_literal(0, 1),
            )
            .with_type_ref(TypeRef::Builtin(BuiltinType::I32)),
            vec![],
            None,
        );
        let block_expr = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

        // act
        let block_scope = global.block(&block_expr);

        // assert
        assert_eq!(block_scope.values.len(), 1);
        assert_eq!(
            block_scope.values.get(&ustr("foo")),
            Some(&TypeRef::Builtin(BuiltinType::I32))
        );
    }

    #[test]
    fn block_tuple_pattern() {
        // arrange
        let global = TypeScope::global();
        let expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(
                0,
                Pattern::Tuple(vec![
                    Pattern::Name(ustr("foo")),
                    Pattern::Discard,
                    Pattern::Name(ustr("bar")),
                ]),
                Expression::tuple(
                    0,
                    vec![
                        Expression::i32_literal(0, 1),
                        Expression::i32_literal(0, 2),
                        Expression::i32_literal(0, 3),
                    ],
                ),
            )
            .with_type_ref(TypeRef::Tuple(TupleType {
                fields: vec![
                    TypeRef::Builtin(BuiltinType::I32),
                    TypeRef::Builtin(BuiltinType::I32),
                    TypeRef::Builtin(BuiltinType::I32),
                ],
            })),
            vec![],
            None,
        );
        let block_expr = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

        // act
        let block_scope = global.block(&block_expr);

        // assert
        assert_eq!(block_scope.values.len(), 2);
        assert_eq!(
            block_scope.values.get(&ustr("foo")),
            Some(&TypeRef::Builtin(BuiltinType::I32))
        );
        assert_eq!(
            block_scope.values.get(&ustr("bar")),
            Some(&TypeRef::Builtin(BuiltinType::I32))
        );
    }
}
