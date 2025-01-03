mod mod_part;
mod uses;
mod function_decl;



#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Visibility {
    Module,
    Public,
}