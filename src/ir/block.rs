use crate::ast::expressions::BlockExpression;
use crate::ast::statement::Statement;
use crate::ir::function::{generate_ir_for_expr, Ir};
use crate::ir::ids::VarId;

pub(super) fn generate_ir_for_block_expr(ir: &mut Ir, expr: &BlockExpression) -> Option<VarId> {
    for stmt in &expr.statements {
        match stmt {
            Statement::Expression(expr) => {
                generate_ir_for_expr(ir, expr);
            }
            Statement::Declaration(_) => todo!(),
        }
    }

    if let Some(return_value) = &expr.value {
        let return_var = generate_ir_for_expr(ir, return_value).unwrap();
        return Some(return_var);
    }

    None
}