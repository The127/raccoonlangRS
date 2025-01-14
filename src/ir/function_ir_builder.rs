use crate::ast::typing::{BuiltinType, TypeRef};
use crate::ir::function::{Block, Function, Instruction};
use crate::ir::ids::{TypeId, VarId};
use ustr::Ustr;

#[derive(Debug)]
pub struct FunctionIrBuilder<'a> {
    function: &'a mut Function,
    active_block: usize,
    next_var_id: usize,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct BlockId(pub(super) usize);

impl FunctionIrBuilder<'_> {
    pub fn new(function: &mut Function) -> FunctionIrBuilder {
        // TODO: should not have any blocks to begin with?
        function.blocks = vec![Block {
            instructions: vec![],
        }];
        function.locals = vec![];

        FunctionIrBuilder {
            function,
            active_block: 0,
            next_var_id: 0,
        }
    }

    pub fn set_name(&mut self, name: Option<Ustr>) {
        self.function.name = name;
    }

    pub fn create_block(&mut self) -> BlockId {
        self.create_block_with_params(vec![])
    }

    pub fn create_block_with_params(&mut self, params: Vec<VarId>) -> BlockId {
        let idx = self.function.blocks.len();

        self.function.blocks.push(Block {
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
        self.function.blocks[self.active_block]
            .instructions
            .push(instr);
    }

    pub fn create_local(&mut self, type_id: TypeId) -> VarId {
        let var_id = VarId::local(self.next_var_id);
        self.next_var_id += 1;
        self.function.locals.push((var_id, type_id));
        var_id
    }

    pub fn map_type(&self, type_ref: &TypeRef) -> TypeId {
        match type_ref {
            TypeRef::Builtin(BuiltinType::Unit) => TypeId::unit(),
            TypeRef::Builtin(BuiltinType::I32) => TypeId::i32(),
            TypeRef::Builtin(BuiltinType::Bool) => TypeId::bool(),
            TypeRef::Tuple(_) => todo!(),
            TypeRef::Unknown => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ir::function::{Block, Function, Instruction};
    use crate::ir::function_ir_builder::{BlockId, FunctionIrBuilder};
    use crate::ir::ids::{TypeId, VarId};

    #[test]
    fn new() {
        // arrange
        let mut func = Function::new();

        // act
        let ir_builder = FunctionIrBuilder::new(&mut func);

        // assert
        assert_eq!(ir_builder.next_var_id, 0);
        assert_eq!(ir_builder.active_block, 0);
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
        let mut func = Function::new();
        let mut ir_builder = FunctionIrBuilder::new(&mut func);

        // act
        let block_id = ir_builder.create_block();

        // assert
        assert_eq!(block_id, BlockId(1));
        assert_eq!(ir_builder.active_block, 0);
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
        let mut func = Function::new();
        let mut ir_builder = FunctionIrBuilder::new(&mut func);

        // act
        let block_id = ir_builder.create_block_with_params(vec![VarId::local(1), VarId::local(2)]);

        // assert
        assert_eq!(block_id, BlockId(1));
        assert_eq!(ir_builder.active_block, 0);
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
        let mut func = Function::new();
        let mut ir_builder = FunctionIrBuilder::new(&mut func);
        let block_id = ir_builder.create_block();

        // act
        ir_builder.set_block(block_id);

        // assert
        assert_eq!(ir_builder.active_block, block_id.0);
    }

    #[test]
    fn get_block() {
        // arrange
        let mut func = Function::new();
        let mut ir_builder = FunctionIrBuilder::new(&mut func);
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
        let mut func = Function::new();
        let mut ir_builder = FunctionIrBuilder::new(&mut func);
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
        let mut func = Function::new();
        let mut ir_builder = FunctionIrBuilder::new(&mut func);

        // act
        let v1 = ir_builder.create_local(TypeId::i32());
        let v2 = ir_builder.create_local(TypeId::bool());
        let v3 = ir_builder.create_local(TypeId::unit());

        // assert
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
        let mut func = Function::new();
        let ir_builder = FunctionIrBuilder::new(&mut func);

        // act
        let t_i32 = ir_builder.map_type(&TypeRef::Builtin(BuiltinType::I32));
        let t_bool = ir_builder.map_type(&TypeRef::Builtin(BuiltinType::Bool));
        let t_unit = ir_builder.map_type(&TypeRef::Builtin(BuiltinType::Unit));

        // assert
        assert_eq!(t_i32, TypeId::i32());
        assert_eq!(t_bool, TypeId::bool());
        assert_eq!(t_unit, TypeId::unit());
    }

    #[test]
    #[should_panic]
    fn map_unknown_type() {
        let mut func = Function::new();
        let ir_builder = FunctionIrBuilder::new(&mut func);

        // act
        ir_builder.map_type(&TypeRef::Unknown);
    }
}
