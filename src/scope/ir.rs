use crate::ir::ids::VarId;
use crate::scope::Scope;

pub type IrVarScope<'a> = Scope<'a, VarId>;