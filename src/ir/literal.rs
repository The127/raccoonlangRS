use crate::ast::expressions::{LiteralExpression, LiteralValue};
use crate::ir::ConstantValue;
use crate::ir::ids::{TypeId, VarId};
use crate::ir::function::Instruction;
use crate::ir::ir_builder::IrBuilder;

pub(super) fn generate_ir_for_literal_expr(ir: &mut IrBuilder, expr: &LiteralExpression) -> VarId {
    match expr.value {
        LiteralValue::Integer(val) => {
            let result = ir.create_local(TypeId::i32());
            ir.instr(Instruction::Const(result, ConstantValue::I32(val)));
            result
        },
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use parameterized::{ide, parameterized};
    use crate::ast::expressions::Expression;
    use crate::ir::function::{Block, Function};
    use super::*;

    ide!();
    #[parameterized(value = {-5, 0, 1, 1024})]
    fn int_literal(value: i32) {
        // arrange
        let expr = assert_matches!(Expression::int_literal(0, value), Expression::Literal(x) => x);
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);

        // act
        let var_id = generate_ir_for_literal_expr(&mut ir, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        let (v1, t1) = function.locals[0];
        assert_eq!(t1, TypeId::i32());
        assert_eq!(v1, var_id);
        assert_eq!(function.blocks, vec![
            Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(value)),
                ]
            }
        ]);
    }
}