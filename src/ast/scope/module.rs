use ustr::Ustr;
use crate::ast::expressions::block::BlockExpression;
use crate::ast::function_decl::FunctionDecl;
use crate::ast::scope::block::BlockScope;
use crate::ast::scope::function::FunctionScope;
use crate::ast::scope::Scope;
use crate::ast::typing::TypeRef;

pub struct ModuleScope {}

impl ModuleScope {
}

impl Scope for ModuleScope {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef> {
        todo!()
    }

    fn block(&self, expr: &BlockExpression) -> BlockScope {
        todo!()
    }
}