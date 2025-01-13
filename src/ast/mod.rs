pub mod file;
pub mod function_decl;
pub mod types;
pub mod expressions;
pub mod statement;
pub mod typing;
pub mod scope;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Visibility {
    Module,
    Public,
}