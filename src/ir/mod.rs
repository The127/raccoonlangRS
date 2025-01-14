mod binary;
mod block;
pub mod function;
pub mod function_ir_builder;
pub mod ids;
mod if_;
mod literal;
mod package;
mod package_ir_builder;
mod access;

#[derive(Debug, Eq, PartialEq)]
pub enum ConstantValue {
    Bool(bool),
    I32(i32),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Visibility {
    Module,
    Public,
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::Expression;
    use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
    use crate::scope::type_::TypeScope;
    use crate::ast::types::{NamedType, Type};
    use crate::ast::typing::function::typecheck_function;
    use crate::ast::Visibility as AstVisibility;
    use crate::errors::Errors;
    use crate::ir::function::{generate_function_ir, Block, Function, FunctionSignature, Instruction};
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::package::Package;
    use crate::ir::package_ir_builder::PackageIrBuilder;
    use crate::ir::ConstantValue;
    use ustr::ustr;

    #[test]
    fn package_with_function() {
        // arrange
        let mut package = Package::new();
        let mut ir = PackageIrBuilder::new(&mut package);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        let mut func_decl = FunctionDecl::new(
            0,
            Some(ustr("foo")),
            AstVisibility::Public,
            vec![
                FunctionParameter::new(0, ustr("a"), Type::Named(NamedType::new(0, vec![ustr("i32")], false))),
                FunctionParameter::new(0, ustr("b"), Type::Named(NamedType::new(0, vec![ustr("i32")], false))),
            ],
            FunctionReturnType {
                type_: Type::Named(NamedType::new(0, vec![ustr("bool")], false)),
                type_ref: None,
            },
            Expression::binary(0, BinaryOperator::Equals, Expression::access(0, ustr("a")), Expression::access(0, ustr("b"))),
        );

        typecheck_function(&mut func_decl, &scope, &mut errors);


        // act
        let func_id = generate_function_ir(&mut ir, &func_decl);

        // assert
        let func = package.get_function(func_id);

        let sig = package.get_signature(func.signature);
        assert_eq!(sig, FunctionSignature {
            parameters: vec![TypeId::i32(), TypeId::i32()],
            return_: TypeId::bool(),
        });

        assert_eq!(func, &Function {
            name: Some(ustr("foo")),
            signature: func.signature, // doesn't matter which id, signature is already asserted
            param_names: Some(vec![ustr("a"), ustr("b")]),
            locals: vec![
                (VarId::local(0), TypeId::i32()),
                (VarId::local(1), TypeId::i32()),
                (VarId::local(2), TypeId::bool()),
            ],
            blocks: vec![
                Block {
                    params: vec![], // TODO: does the first block get the function params as args?
                    instructions: vec![
                        Instruction::Const(VarId::local(0), ConstantValue::I32(1)), // TODO: use function param
                        Instruction::Const(VarId::local(1), ConstantValue::I32(2)), // TODO: use function param
                        Instruction::Equals(VarId::local(2), VarId::local(0), VarId::local(1)),
                        Instruction::Return(VarId::local(2)),
                    ]
                }
            ],
        });

    }
}
