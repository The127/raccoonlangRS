use crate::ast::expressions::{BinaryOperator, Expression, ExpressionKind};
use crate::ir::function::{generate_ir_for_expr, Instruction};
use crate::ir::ids::{TypeId, VarId};
use crate::ir::ir_builder::IrBuilder;

pub(super) fn generate_ir_for_binary_expr(ir: &mut IrBuilder, expr: &Expression) -> VarId {
    let result_type = ir.map_type(expr.type_ref.as_ref().unwrap());
    let result_var = ir.create_local(result_type);

    match &expr.kind {
        ExpressionKind::Binary(binary) => {
            let left = generate_ir_for_expr(ir, binary.left.as_ref()).unwrap();
            let right = generate_ir_for_expr(ir, binary.right.as_ref()).unwrap();
            let instr = match binary.op {
                BinaryOperator::Plus => Instruction::Add(result_var, left, right),
                BinaryOperator::Minus => Instruction::Sub(result_var, left, right),
                BinaryOperator::Equals => Instruction::Equals(result_var, left, right),
                BinaryOperator::NotEquals => Instruction::NotEquals(result_var, left, right),
                BinaryOperator::LessThan => Instruction::LessThan(result_var, left, right),
                BinaryOperator::GreaterThan => Instruction::GreaterThan(result_var, left, right),
                BinaryOperator::LessThanOrEquals => {
                    Instruction::LessThanOrEquals(result_var, left, right)
                }
                BinaryOperator::GreaterThanOrEquals => {
                    Instruction::GreaterThanOrEquals(result_var, left, right)
                }
            };
            ir.instr(instr);
        }
        _ => unreachable!(),
    }

    result_var
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::{BinaryOperator, Expression};
    use crate::ast::typing::{calculate_expression_type, Scope};
    use crate::ir::function::{Block, Function};
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use parameterized::ide;
    use paste::paste;

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
                    let scope = Scope {};
                    calculate_expression_type(&mut expr, &scope);

                    let mut function = Function::new();
                    let mut ir = IrBuilder::new(&mut function);

                    // act
                    let result_var = generate_ir_for_binary_expr(&mut ir, &expr);

                    // assert
                    assert_eq!(function.locals.len(), 3);
                    assert_matches!(&function.blocks[..], [
                       Block {
                            params,
                            instructions
                        }
                    ] => {
                        assert!(params.is_empty());
                        assert_matches!(&instructions[..], [
                            Instruction::Const(v1a, ConstantValue::I32(42)),
                            Instruction::Const(v2a, ConstantValue::I32(69)),
                            Instruction::$instr(v3, v1b, v2b),
                        ] => {
                            assert_eq!(v1a, v1b);
                            assert_eq!(v2a, v2b);

                            let result_type = function.locals.iter().find(|(var, type_)| var == v3 );
                            assert_eq!(result_type, Some(&(*v3, TypeId::$result_type())));
                        });
                    });
                }
            }
        };
    }

    binary_ir_test!(add, Plus, Add, i32);
    binary_ir_test!(sub, Minus, Sub, i32);
    binary_ir_test!(eq, Equals, Equals, bool);
    binary_ir_test!(ne, NotEquals, NotEquals, bool);
    binary_ir_test!(lt, LessThan, LessThan, bool);
    binary_ir_test!(gt, GreaterThan, GreaterThan, bool);
    binary_ir_test!(lte, LessThanOrEquals, LessThanOrEquals, bool);
    binary_ir_test!(gte, GreaterThanOrEquals, GreaterThanOrEquals, bool);

}
