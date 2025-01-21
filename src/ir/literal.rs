use crate::ast::expressions::literal::LiteralValue;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ir::function::Instruction;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::ir::ConstantValue;
use assert_matches::assert_matches;

pub(super) fn generate_ir_for_literal_expr(
    ir: &mut FunctionIrBuilder,
    target: VarId,
    expr: &Expression,
) {
    let literal = assert_matches!(&expr.kind, ExpressionKind::Literal(x) => x);
    match &literal.value {
        LiteralValue::I32(val) => {
            ir.instr(Instruction::Const(target, ConstantValue::I32(*val)));
        }
        LiteralValue::U32(val) => {
            ir.instr(Instruction::Const(target, ConstantValue::U32(*val)));
        },
        LiteralValue::F32(val) => {
            ir.instr(Instruction::Const(target, ConstantValue::F32(*val)));
        },
        LiteralValue::Boolean(val) => {
            ir.instr(Instruction::Const(target, ConstantValue::Bool(*val)));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ir::function::Block;
    use crate::ir::test::IrTestEnv;
    use parameterized::{ide, parameterized};
    use crate::ir::ids::TypeId;

    ide!();
    #[parameterized(value = {-5, 0, 1, 1024})]
    fn i32_literal(value: i32) {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::i32_literal(0, value);
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::i32());

        // act
        generate_ir_for_literal_expr(&mut env.function_ir_builder, result_var, &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![Instruction::Const(result_var, ConstantValue::I32(value)),]
            }]
        );
    }

    #[parameterized(value = {0, 1, 5, 1024})]
    fn u32_literal(value: u32) {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::u32_literal(0, value);
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::u32());

        // act
        generate_ir_for_literal_expr(&mut env.function_ir_builder, result_var, &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![Instruction::Const(result_var, ConstantValue::U32(value)),]
            }]
        );
    }

    #[parameterized(value = {0.0, 1.2, 5.3, 1024.12345})]
    fn f32_literal(value: f32) {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::f32_literal(0, value);
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::u32());

        // act
        generate_ir_for_literal_expr(&mut env.function_ir_builder, result_var, &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![Instruction::Const(result_var, ConstantValue::F32(value)),]
            }]
        );
    }

    #[parameterized(value = {true, false})]
    fn bool_literal(value: bool) {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::bool_literal(0, value);
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::bool());

        // act
        generate_ir_for_literal_expr(&mut env.function_ir_builder, result_var, &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![Instruction::Const(result_var, ConstantValue::Bool(value)),]
            }]
        );
    }
}
