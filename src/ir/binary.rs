use crate::ast::expressions::binary::BinaryOperator;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ir::function::{generate_ir_for_expr_as_var, Instruction};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::scope::ir::IrVarScope;
use assert_matches::assert_matches;
use crate::errors::Errors;

pub(super) fn generate_ir_for_binary_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: VarId,
    expr: &Expression,
    errors: &mut Errors,
) {
    let binary = assert_matches!(&expr.kind, ExpressionKind::Binary(x) => x);


    let left = generate_ir_for_expr_as_var(ir, scope, &binary.left, errors);
    let right = generate_ir_for_expr_as_var(ir, scope, &binary.right, errors);

    let instr = match *binary.op {
        BinaryOperator::Add => Instruction::Add(target, left, right),
        BinaryOperator::Sub => Instruction::Sub(target, left, right),
        BinaryOperator::Mul => Instruction::Mul(target, left, right),
        BinaryOperator::Div => Instruction::Div(target, left, right),
        BinaryOperator::Equals => Instruction::Equals(target, left, right),
        BinaryOperator::NotEquals => Instruction::NotEquals(target, left, right),
        BinaryOperator::LessThan => Instruction::LessThan(target, left, right),
        BinaryOperator::GreaterThan => Instruction::GreaterThan(target, left, right),
        BinaryOperator::LessThanOrEquals => Instruction::LessThanOrEquals(target, left, right),
        BinaryOperator::GreaterThanOrEquals => {
            Instruction::GreaterThanOrEquals(target, left, right)
        }
    };
    ir.instr(instr);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::Expression;
    use crate::ir::function::Block;
    use crate::ir::ids::TypeId;
    use crate::ir::test::IrTestEnv;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use parameterized::ide;
    use paste::paste;
    use crate::parser::ToSpanned;

    ide!();

    macro_rules! binary_ir_test {
        ($name:ident, $op:ident, $instr:ident, $result_type:ident) => {
            paste! {
                #[test]
                fn [<op_$name>]() {
                    // arrange
                    let mut errors = Errors::new();
                    let mut env = IrTestEnv::new();
                    let mut expr = Expression::binary(
                        0,
                        BinaryOperator::$op.spanned_empty(),
                        Expression::i32_literal(0, 42),
                        Expression::i32_literal(0, 69),
                    );
                    env.typecheck_expression(&mut expr);
                    let result_var = env.function_ir_builder.create_local(TypeId::$result_type());

                    // act
                    generate_ir_for_binary_expr(&mut env.function_ir_builder, &env.ir_var_scope, result_var, &expr, &mut errors);

                    // assert
                    let func = env.get_function();
                    assert_eq!(func.locals.len(), 3);
                    assert_matches!(&func.blocks[..], [
                       Block {
                            instructions
                        }
                    ] => {
                        assert_matches!(&instructions[..], [
                            Instruction::Const(v1a, ConstantValue::I32(42)),
                            Instruction::Const(v2a, ConstantValue::I32(69)),
                            Instruction::$instr(v3, v1b, v2b),
                        ] => {
                            assert_eq!(v1a, v1b);
                            assert_eq!(v2a, v2b);
                            assert_eq!(v3, &result_var);
                        });
                    });
                }
            }
        };
    }

    binary_ir_test!(add, Add, Add, i32);
    binary_ir_test!(sub, Sub, Sub, i32);
    binary_ir_test!(mul, Mul, Mul, i32);
    binary_ir_test!(div, Div, Div, i32);
    binary_ir_test!(eq, Equals, Equals, bool);
    binary_ir_test!(ne, NotEquals, NotEquals, bool);
    binary_ir_test!(lt, LessThan, LessThan, bool);
    binary_ir_test!(gt, GreaterThan, GreaterThan, bool);
    binary_ir_test!(lte, LessThanOrEquals, LessThanOrEquals, bool);
    binary_ir_test!(gte, GreaterThanOrEquals, GreaterThanOrEquals, bool);
}
