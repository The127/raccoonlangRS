mod file;
mod uses;
mod function_decl;
mod types;
mod expressions;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Visibility {
    Module,
    Public,
}