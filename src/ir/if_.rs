use crate::ast::expressions::IfExpression;
use crate::ir::function::{generate_ir_for_expr, BranchTarget, Instruction, Ir};
use crate::ir::ids::{TypeId, VarId};

pub(super) fn generate_ir_for_if_expr(ir: &mut Ir, expr: &IfExpression) -> Option<VarId> {
    let cond_var = generate_ir_for_expr(ir, expr.condition.as_ref().unwrap()).unwrap();
    let current_block_idx = ir.blocks.len() - 1;
    let then_idx = current_block_idx + 1;
    let else_idx = current_block_idx + 2;
    let final_idx = current_block_idx + 3;
    ir.push_instr(Instruction::BranchIf(cond_var, BranchTarget {
        block_index: then_idx,
        arguments: vec![],
    }, BranchTarget {
        block_index: else_idx,
        arguments: vec![],
    }));

    ir.push_block(vec![]);
    let then_result_var = generate_ir_for_expr(ir, expr.then.as_ref().unwrap()).unwrap();
    ir.push_instr(Instruction::Branch(BranchTarget { block_index: final_idx, arguments: vec![then_result_var] }));

    ir.push_block(vec![]);
    let else_result_var = generate_ir_for_expr(ir, expr.else_.as_ref().unwrap()).unwrap();
    ir.push_instr(Instruction::Branch(BranchTarget { block_index: final_idx, arguments: vec![else_result_var] }));

    let result_var = ir.new_var(TypeId::i32());

    ir.push_block(vec![result_var]);

    Some(result_var)
}