use std::fmt::{Display, Formatter};

mod binary;
mod block;
pub mod function;
pub mod function_ir_builder;
pub mod ids;
mod if_;
mod literal;
pub mod package;
pub mod package_ir_builder;
mod access;
mod tuple;
pub mod graph;

#[derive(Debug, PartialEq)]
pub enum ConstantValue {
    Bool(bool),
    I32(i32),
    U32(u32),
    F32(f32),
}

impl Eq for ConstantValue {} // TODO: this is WRONG, need to also implement PartialEq correctly, maybe figure out some way to do that without having to do it manually in every type that contains a float

impl Display for ConstantValue {
    #[mutants::skip]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantValue::Bool(val) => write!(f, "const.bool {}", val),
            ConstantValue::I32(val) => write!(f, "const.i32 {}", val),
            ConstantValue::U32(val) => write!(f, "const.u32 {}", val),
            ConstantValue::F32(val) => write!(f, "const.f32 {}", val),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Visibility {
    Module,
    Public,
}

#[cfg(test)]
mod test {
    use crate::ast::function_decl::{FunctionDecl, FunctionParameter, FunctionReturnType};
    use crate::ast::path::Path;
    use crate::ast::types::{NamedType, Type};
    use crate::ast::typing::function::typecheck_function_interior;
    use crate::ast::typing::typecheck_expression;
    use crate::ast::Visibility as AstVisibility;
    use crate::errors::Errors;
    use crate::ir::function::{generate_function_ir, Block, Function, FunctionSignature, Instruction};
    use crate::ir::function_ir_builder::FunctionIrBuilder;
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::package::Package;
    use crate::ir::package_ir_builder::{FunctionId, PackageIrBuilder};
    use crate::parser::ToSpanned;
    use crate::scope::ir::IrVarScope;
    use crate::scope::type_::TypeScope;
    use std::mem::MaybeUninit;
    use std::ptr::addr_of_mut;
    use ustr::ustr;
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::Expression;

    pub struct IrTestEnv {
        pub package: Package,
        pub package_ir_builder: PackageIrBuilder<'static>,
        pub function_id: FunctionId,
        pub function_ir_builder: FunctionIrBuilder<'static, 'static>,
        pub errors: Errors,
        pub type_scope: TypeScope<'static>,
        pub ir_var_scope: IrVarScope<'static>,
    }

    impl IrTestEnv {
        pub fn new() -> Box<IrTestEnv> {
            let mut uninit = Box::new(MaybeUninit::<IrTestEnv>::zeroed());
            unsafe {
                let ptr = uninit.as_mut_ptr();
                addr_of_mut!((*ptr).package).write(Package::new());
                addr_of_mut!((*ptr).package_ir_builder).write(PackageIrBuilder::new(&mut (*ptr).package));
                addr_of_mut!((*ptr).function_id).write((*ptr).package_ir_builder.create_function());
                addr_of_mut!((*ptr).function_ir_builder).write(FunctionIrBuilder::new(
                    &mut (*ptr).package_ir_builder,
                    (*ptr).function_id,
                ));
                addr_of_mut!((*ptr).errors).write(Errors::new());
                addr_of_mut!((*ptr).type_scope).write(TypeScope::new());
                addr_of_mut!((*ptr).ir_var_scope).write(IrVarScope::new());
                uninit.assume_init()
            }
        }

        pub fn typecheck_expression(&mut self, expr: &mut Expression) {
            typecheck_expression(expr, &self.type_scope, &mut self.errors);
            self.errors.assert_empty();
        }

        pub fn typecheck_function(&mut self, func: &mut FunctionDecl) {
            typecheck_function_interior(func, &self.type_scope, &mut self.errors);
            self.errors.assert_empty();
        }

        pub fn get_function(&self) -> &Function {
            self.package.get_function(self.function_id)
        }
    }

    // #[test]
    fn package_with_function() {
        // arrange
        let mut env = IrTestEnv::new();

        let mut package = Package::new();
        let mut ir = PackageIrBuilder::new(&mut package);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        let mut func_decl = FunctionDecl::new(
            0,
            Some(ustr("foo")),
            AstVisibility::Public,
            vec![
                FunctionParameter::new(0, ustr("a"), Type::Named(NamedType::new(0, Path::name("i32")))),
                FunctionParameter::new(0, ustr("b"), Type::Named(NamedType::new(0, Path::name("i32")))),
            ],
            FunctionReturnType {
                type_: Type::Named(NamedType::new(0, Path::name("bool"))),
                type_ref: None,
            },
            Expression::binary(0, BinaryOperator::Equals.spanned_empty(), Expression::access(0, Path::name("a")), Expression::access(0, Path::name("b"))),
        );

        env.typecheck_function(&mut func_decl);

        let func_id = ir.create_function();
        let mut func_ir = ir.function_builder(func_id);

        // act
        generate_function_ir(&mut func_ir, &func_decl, &mut errors);

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
                (VarId::local(0), TypeId::bool()),
            ],
            blocks: vec![
                Block {
                    instructions: vec![
                        Instruction::Equals(VarId::local(0), VarId::param(0), VarId::param(1)),
                        Instruction::Return(VarId::local(0)),
                    ]
                }
            ],
        });

    }
}
