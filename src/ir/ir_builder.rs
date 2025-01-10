use crate::ir::function::{Block, Function, Instruction};
use crate::ir::ids::{TypeId, VarId};

#[derive(Debug, Eq, PartialEq)]
pub struct IrBuilder<'a> {
    function: &'a mut Function,
    active_block: usize,
    next_var_id: usize,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct BlockId(pub(super) usize);

impl IrBuilder<'_> {
    pub fn new<'a >(function: &'a mut Function) -> IrBuilder<'a> {
        function.blocks = vec![Block {
            params: vec![],
            instructions: vec![],
        }];
        function.locals = vec![];
        
        IrBuilder::<'a> {
            function,
            active_block: 0,
            next_var_id: 0,
        }
    }

    pub fn create_block(&mut self) -> BlockId {
        self.create_block_with_params(vec![])
    }

    pub fn create_block_with_params(&mut self, params: Vec<VarId>) -> BlockId {
        let idx = self.function.blocks.len();

        self.function.blocks.push(Block {
            params: params,
            instructions: vec![],
        });

        BlockId(idx)
    }
    
    pub fn set_block(&mut self, block: BlockId) {
        self.active_block = block.0;
    }

    pub fn instr(&mut self, instr: Instruction) {
        self.function.blocks[self.active_block].instructions.push(instr);
    }

    pub fn create_local(&mut self, type_id: TypeId) -> VarId {
        let var_id = VarId::local(self.next_var_id);
        self.next_var_id += 1;
        self.function.locals.push((var_id, type_id));
        var_id
    }
}