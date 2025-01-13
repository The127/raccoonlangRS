use ustr::Ustr;
use crate::ast::expressions::block::BlockExpression;
use crate::ast::scope::block::BlockScope;
use crate::ast::scope::module::ModuleScope;
use crate::ast::scope::Scope;
use crate::ast::typing::TypeRef;

pub struct GlobalScope {}

impl GlobalScope {
    pub fn new() -> Self {
        Self {}
    }
}

impl Scope for GlobalScope {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef> {
        None
    }

    fn block<'a>(&'a self, expr: &BlockExpression) -> BlockScope<'a> {
        BlockScope::new(self, expr)
    }
}
