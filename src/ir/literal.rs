use crate::ast::expressions::{LiteralExpression, LiteralValue};
use crate::ir::ConstantValue;
use crate::ir::ids::{TypeId, VarId};
use crate::ir::function::{Instruction, Ir};

pub(super) fn generate_ir_for_literal_expr(ir: &mut Ir, expr: &LiteralExpression) -> VarId {
    match expr.value {
        LiteralValue::Integer(val) => {
            let result = ir.new_var(TypeId::i32());
            ir.push_instr(Instruction::Const(result, ConstantValue::I32(val)));
            result
        },
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use parameterized::{ide, parameterized};
    use crate::ast::expressions::Expression;
    use crate::ir::function::Block;
    use super::*;

    ide!();
    #[parameterized(value = {-5, 0, 1, 1024})]
    fn int_literal(value: i32) {
        // arrange
        let expr = assert_matches!(Expression::int_literal(0, value), Expression::Literal(x) => x);
        let mut ir = Ir::new();

        // act
        let var_id = generate_ir_for_literal_expr(&mut ir, &expr);

        // assert
        assert_eq!(ir.locals.len(), 1);
        let (v1, t1) = ir.locals[0];
        assert_eq!(t1, TypeId::i32());
        assert_eq!(v1, var_id);
        assert_eq!(ir.blocks, vec![
            Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(value)),
                ]
            }
        ]);
    }
}