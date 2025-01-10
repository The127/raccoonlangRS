use crate::ast::expressions::IfExpression;
use crate::ir::function::{generate_ir_for_expr, BranchTarget, Instruction};
use crate::ir::ids::{TypeId, VarId};
use crate::ir::ir_builder::IrBuilder;

pub(super) fn generate_ir_for_if_expr(ir: &mut IrBuilder, expr: &IfExpression) -> Option<VarId> {
    let cond_var = generate_ir_for_expr(ir, expr.condition.as_ref().unwrap()).unwrap();

    let result_var = ir.new_var(TypeId::i32());

    let then_block = ir.create_block();
    let else_block = ir.create_block();
    let final_block = ir.create_block_with_params(vec![result_var]);

    ir.push_instr(Instruction::BranchIf(
        cond_var,
        BranchTarget::new(then_block),
        BranchTarget::new(else_block),
    ));

    ir.set_block(then_block);
    let then_result_var = generate_ir_for_expr(ir, expr.then.as_ref().unwrap()).unwrap();
    ir.push_instr(Instruction::Branch(BranchTarget::with_args(
        final_block,
        vec![then_result_var],
    )));

    ir.set_block(else_block);
    let else_result_var = generate_ir_for_expr(ir, expr.else_.as_ref().unwrap()).unwrap();
    ir.push_instr(Instruction::Branch(BranchTarget::with_args(
        final_block,
        vec![else_result_var],
    )));

    ir.set_block(final_block);

    Some(result_var)
}
