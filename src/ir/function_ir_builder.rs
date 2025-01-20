use crate::ast::typing::TypeRef;
use crate::ir::function::{Block, Function, Instruction};
use crate::ir::ids::{SignatureId, TypeId, VarId};
use crate::ir::package_ir_builder::{FunctionId, PackageIrBuilder};
use ustr::Ustr;

#[derive(Debug)]
pub struct FunctionIrBuilder<'a, 'b> {
    package_ir_builder: &'a mut PackageIrBuilder<'b>,
    function_id: FunctionId,
    active_block: usize,
    next_var_id: usize,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct BlockId(pub(super) usize);

impl FunctionIrBuilder<'_, '_> {
    pub fn new<'a, 'b>(package_ir_builder: &'a mut PackageIrBuilder<'b>, function_id: FunctionId) -> FunctionIrBuilder<'a, 'b> {
        let builder = FunctionIrBuilder {
            package_ir_builder,
            function_id,
            active_block: 0,
            next_var_id: 0,
        };

        builder
    }

    fn get_func(&mut self) -> &mut Function {
        self.package_ir_builder.package.functions.get_mut(self.function_id.0).unwrap()
    }

    pub fn set_name(&mut self, name: Option<Ustr>) {
        self.get_func().name = name;
    }

    pub fn set_param_names(&mut self, names: Option<Vec<Ustr>>) {
        self.get_func().param_names = names;
    }

    pub fn set_signature(&mut self, signature_id: SignatureId) {
        self.get_func().signature = signature_id;
    }

    pub fn create_block(&mut self) -> BlockId {
        self.create_block_with_params(vec![])
    }

    pub fn create_block_with_params(&mut self, params: Vec<VarId>) -> BlockId {
        let idx = self.get_func().blocks.len();

        self.get_func().blocks.push(Block {
            instructions: vec![],
        });

        BlockId(idx)
    }

    pub fn set_block(&mut self, block: BlockId) {
        self.active_block = block.0;
    }

    pub fn get_block(&self) -> BlockId {
        BlockId(self.active_block)
    }

    pub fn instr(&mut self, instr: Instruction) {
        let active_block = self.active_block;
        self.get_func().blocks.get_mut(active_block)
            .expect("unknown block")
            .instructions
            .push(instr);
    }

    pub fn create_local(&mut self, type_id: TypeId) -> VarId {
        let var_id = VarId::local(self.next_var_id);
        self.next_var_id += 1;
        self.get_func().locals.push((var_id, type_id));
        var_id
    }

    pub fn map_type(&mut self, type_ref: &TypeRef) -> TypeId {
        self.package_ir_builder.map_type(type_ref)
    }

    pub fn map_signature(&mut self, return_: &TypeRef, params: Vec<&TypeRef>) -> SignatureId {
        let return_typeid = self.map_type(return_);
        let param_typeids = params.iter().map(|x| self.map_type(x)).collect();
        self.package_ir_builder.map_signature(return_typeid, param_typeids)
    }
}

#[cfg(test)]
mod test {
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ir::function::{Block, Instruction};
    use crate::ir::function_ir_builder::BlockId;
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::package::Package;
    use crate::ir::package_ir_builder::PackageIrBuilder;

    #[test]
    fn new() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();

        // act
        let ir_builder = package_ir.function_builder(func_id);

        // assert
        assert_eq!(ir_builder.next_var_id, 0);
        assert_eq!(ir_builder.active_block, 0);
        let func = package.get_function(func_id);
        assert!(func.locals.is_empty());
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![]
            }]
        );
    }

    #[test]
    fn create_block() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);

        // act
        let block_id = ir_builder.create_block();

        // assert
        assert_eq!(block_id, BlockId(1));
        assert_eq!(ir_builder.active_block, 0);
        let func = package.get_function(func_id);
        assert_eq!(
            func.blocks,
            vec![
                Block {
                    instructions: vec![]
                },
                Block {
                    instructions: vec![]
                },
            ]
        );
    }

    #[test]
    fn create_block_with_params() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);

        // act
        let block_id = ir_builder.create_block_with_params(vec![VarId::local(1), VarId::local(2)]);

        // assert
        assert_eq!(block_id, BlockId(1));
        assert_eq!(ir_builder.active_block, 0);
        let func = package.get_function(func_id);
        assert_eq!(
            func.blocks,
            vec![
                Block {
                    instructions: vec![]
                },
                Block {
                    instructions: vec![]
                },
            ]
        );
    }

    #[test]
    fn set_block() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);
        let block_id = ir_builder.create_block();

        // act
        ir_builder.set_block(block_id);

        // assert
        assert_eq!(ir_builder.active_block, block_id.0);
    }

    #[test]
    fn get_block() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);
        let second_block = ir_builder.create_block();

        // act
        let initial_block = ir_builder.get_block();
        ir_builder.set_block(second_block);
        let block_after_set = ir_builder.get_block();

        // assert
        assert_eq!(initial_block, BlockId(0));
        assert_eq!(second_block, block_after_set);
    }

    #[test]
    fn instr() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);
        let second_block = ir_builder.create_block();
        let v1 = ir_builder.create_local(TypeId::i32());
        let v2 = ir_builder.create_local(TypeId::i32());
        let v3 = ir_builder.create_local(TypeId::i32());

        // act
        ir_builder.instr(Instruction::Add(v1, v2, v3));
        ir_builder.instr(Instruction::Sub(v1, v2, v3));
        ir_builder.set_block(second_block);
        ir_builder.instr(Instruction::Mul(v1, v2, v3));
        ir_builder.instr(Instruction::Div(v1, v2, v3));

        // assert
        let func = package.get_function(func_id);
        assert_eq!(
            func.blocks,
            vec![
                Block {
                    instructions: vec![Instruction::Add(v1, v2, v3), Instruction::Sub(v1, v2, v3),],
                },
                Block {
                    instructions: vec![Instruction::Mul(v1, v2, v3), Instruction::Div(v1, v2, v3),],
                },
            ]
        )
    }

    #[test]
    fn create_local() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);

        // act
        let v1 = ir_builder.create_local(TypeId::i32());
        let v2 = ir_builder.create_local(TypeId::bool());
        let v3 = ir_builder.create_local(TypeId::unit());

        // assert
        let func = package.get_function(func_id);
        assert_eq!(
            func.locals,
            vec![
                (v1, TypeId::i32()),
                (v2, TypeId::bool()),
                (v3, TypeId::unit()),
            ]
        );
    }

    #[test]
    fn map_type() {
        // arrange
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);

        // act
        let t_i32 = ir_builder.map_type(&TypeRef::Builtin(BuiltinType::I32));
        let t_bool = ir_builder.map_type(&TypeRef::Builtin(BuiltinType::Bool));
        let t_unit = ir_builder.map_type(&TypeRef::Builtin(BuiltinType::Unit));
        let t_tuple = ir_builder.map_type(&TypeRef::tuple(vec![
            TypeRef::Builtin(BuiltinType::I32),
            TypeRef::Builtin(BuiltinType::I32),
        ]));

        // assert
        assert_eq!(t_i32, TypeId::i32());
        assert_eq!(t_bool, TypeId::bool());
        assert_eq!(t_unit, TypeId::unit());
        // TODO: assert the tuple type is correct
    }

    #[test]
    #[should_panic]
    fn map_unknown_type() {
        let mut package = Package::new();
        let mut package_ir = PackageIrBuilder::new(&mut package);
        let func_id = package_ir.create_function();
        let mut ir_builder = package_ir.function_builder(func_id);

        // act
        ir_builder.map_type(&TypeRef::Unknown);
    }
}
