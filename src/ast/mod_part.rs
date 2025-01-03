use crate::ast::function_decl::FunctionDecl;
use crate::ast::uses::Uses;

pub struct ModPart {
    pub uses: Uses,
    pub functions: Vec<FunctionDecl>,
}

#[cfg(test)]
mod test {

    #[test]
    fn transform_idk(){

    }
}