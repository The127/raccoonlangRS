use crate::ast::expressions::binary::BinaryOperator;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ir::function::{generate_ir_for_expr, Instruction};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use assert_matches::assert_matches;

pub(super) fn generate_ir_for_binary_expr(
    ir: &mut FunctionIrBuilder,
    target: VarId,
    expr: &Expression,
) {
    let binary = assert_matches!(&expr.kind, ExpressionKind::Binary(x) => x);

    let left = ir.create_local(ir.map_type(binary.left.type_ref.as_ref().unwrap()));
    generate_ir_for_expr(ir, Some(left), &binary.left);
    let right = ir.create_local(ir.map_type(binary.right.type_ref.as_ref().unwrap()));
    generate_ir_for_expr(ir, Some(right), &binary.right);

    let instr = match binary.op {
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
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::function::{Block, Function};
    use crate::ir::ids::TypeId;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use parameterized::ide;
    use paste::paste;
    use crate::scope::type_::TypeScope;

    ide!();

    macro_rules! binary_ir_test {
        ($name:ident, $op:ident, $instr:ident, $result_type:ident) => {
            paste! {
                #[test]
                fn [<op_$name>]() {
                    // arrange
                    let mut expr = Expression::binary(
                        0,
                        BinaryOperator::$op,
                        Expression::int_literal(0, 42),
                        Expression::int_literal(0, 69),
                    );
                    let scope = TypeScope::new();

                    let mut errors = Errors::new();
                    typecheck_expression(&mut expr, &scope, &mut errors);
                    assert!(errors.get_errors().is_empty());

                    let mut function = Function::new();
                    let mut ir = FunctionIrBuilder::new(&mut function);
                    let result_var = ir.create_local(TypeId::$result_type());

                    // act
                    generate_ir_for_binary_expr(&mut ir, result_var, &expr);

                    // assert
                    assert_eq!(function.locals.len(), 3);
                    assert_matches!(&function.blocks[..], [
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
