mod add;
mod ids;
mod literal;
mod if_;
mod block;
mod function;

#[derive(Debug, Eq, PartialEq)]
pub enum ConstantValue {
    I32(i32),
}
