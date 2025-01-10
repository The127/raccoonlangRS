use crate::ir::function::{Block, Instruction};
use crate::ir::ids::{TypeId, VarId};

mod add;
mod ids;
mod literal;
mod if_;
mod block;
pub mod function;
pub mod ir_builder;

#[derive(Debug, Eq, PartialEq)]
pub enum ConstantValue {
    I32(i32),
}


#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Visibility {
    Module,
    Public,
}
