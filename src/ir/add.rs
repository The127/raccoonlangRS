use crate::ast::expressions::{AddExpression, AddExpressionOperator};
use crate::ir::function::{generate_ir_for_expr, Instruction, Ir};
use crate::ir::ids::{TypeId, VarId};

pub(super) fn generate_ir_for_add_expr(ir: &mut Ir, expr: &AddExpression) -> VarId {
    let mut current = generate_ir_for_expr(ir, &expr.left).unwrap();

    for follow in &expr.follows {
        let operand = generate_ir_for_expr(ir, follow.operand.as_ref().unwrap()).unwrap();
        let result = ir.new_var(TypeId::i32());
        let instr = match follow.operator {
            AddExpressionOperator::OpPlus => Instruction::Add(result, current, operand),
            AddExpressionOperator::OpMinus => Instruction::Sub(result, current, operand),
        };
        ir.push_instr(instr);
        current = result
    }

    current
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::{AddExpressionOperator, Expression};
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use crate::ir::function::Block;

    #[test]
    fn simple_addition() {
        // arrange
        let add_expr = assert_matches!(Expression::add(
            0,
            Expression::int_literal(0, 42),
            vec![(
                AddExpressionOperator::OpPlus,
                Expression::int_literal(0, 69),
            )],
        ), Expression::Add(x) => x);
        let mut ir = Ir::new();

        // act
        let result_var = generate_ir_for_add_expr(&mut ir, &add_expr);

        // assert
        assert_eq!(ir.locals.len(), 3);
        let (v1, t1) = ir.locals[0];
        let (v2, t2) = ir.locals[1];
        let (v3, t3) = ir.locals[2];
        assert_eq!(v3, result_var);
        assert_eq!(t1, TypeId::i32());
        assert_eq!(t2, TypeId::i32());
        assert_eq!(t3, TypeId::i32());
        assert_eq!(
            ir.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(42)),
                    Instruction::Const(v2, ConstantValue::I32(69)),
                    Instruction::Add(v3, v1, v2),
                ]
            }]
        );
    }

    #[test]
    fn simple_subtraction() {
        // arrange
        let add_expr = assert_matches!(Expression::add(
            0,
            Expression::int_literal(0, 42),
            vec![(
                AddExpressionOperator::OpMinus,
                Expression::int_literal(0, 69),
            )],
        ), Expression::Add(x) => x);
        let mut ir = Ir::new();

        // act
        let result_var = generate_ir_for_add_expr(&mut ir, &add_expr);

        // assert
        assert_eq!(ir.locals.len(), 3);
        let (v1, t1) = ir.locals[0];
        let (v2, t2) = ir.locals[1];
        let (v3, t3) = ir.locals[2];
        assert_eq!(v3, result_var);
        assert_eq!(t1, TypeId::i32());
        assert_eq!(t2, TypeId::i32());
        assert_eq!(t3, TypeId::i32());
        assert_eq!(
            ir.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(42)),
                    Instruction::Const(v2, ConstantValue::I32(69)),
                    Instruction::Sub(v3, v1, v2),
                ]
            }]
        );
    }

    #[test]
    fn multi_addition_subtraction() {
        // arrange
        let add_expr = assert_matches!(Expression::add(
            0,
            Expression::int_literal(0, 42),
            vec![
                (
                    AddExpressionOperator::OpMinus,
                    Expression::int_literal(0, 69),
                ),
                (
                    AddExpressionOperator::OpPlus,
                    Expression::int_literal(0, 23),
                ),
                (
                    AddExpressionOperator::OpMinus,
                    Expression::int_literal(0, 7),
                ),
            ],
        ), Expression::Add(x) => x);
        let mut ir = Ir::new();

        // act
        let result_var = generate_ir_for_add_expr(&mut ir, &add_expr);

        // assert
        assert_eq!(ir.locals.len(), 7);
        let (v1, t1) = ir.locals[0];
        let (v2, t2) = ir.locals[1];
        let (v3, t3) = ir.locals[2];
        let (v4, t4) = ir.locals[3];
        let (v5, t5) = ir.locals[4];
        let (v6, t6) = ir.locals[5];
        let (v7, t7) = ir.locals[6];
        assert_eq!(v7, result_var);
        assert_eq!(t1, TypeId::i32());
        assert_eq!(t2, TypeId::i32());
        assert_eq!(t3, TypeId::i32());
        assert_eq!(t4, TypeId::i32());
        assert_eq!(t5, TypeId::i32());
        assert_eq!(t6, TypeId::i32());
        assert_eq!(t7, TypeId::i32());
        assert_eq!(
            ir.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(42)),
                    Instruction::Const(v2, ConstantValue::I32(69)),
                    Instruction::Sub(v3, v1, v2),
                    Instruction::Const(v4, ConstantValue::I32(23)),
                    Instruction::Add(v5, v3, v4),
                    Instruction::Const(v6, ConstantValue::I32(7)),
                    Instruction::Sub(v7, v5, v6),
                ]
            }]
        );
    }
}
