mod binary;
mod ids;
mod literal;
mod if_;
mod block;
pub mod function;
pub mod ir_builder;

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
