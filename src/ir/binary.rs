use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::expressions::binary::BinaryOperator;
use crate::ir::function::{generate_ir_for_expr, Instruction};
use crate::ir::ids::{TypeId, VarId};
use crate::ir::function_ir_builder::FunctionIrBuilder;

pub(super) fn generate_ir_for_binary_expr(ir: &mut FunctionIrBuilder, expr: &Expression) -> VarId {
    let result_type = ir.map_type(expr.type_ref.as_ref().unwrap());
    let result_var = ir.create_local(result_type);

    match &expr.kind {
        ExpressionKind::Binary(binary) => {
            let left = generate_ir_for_expr(ir, binary.left.as_ref()).unwrap();
            let right = generate_ir_for_expr(ir, binary.right.as_ref()).unwrap();
            let instr = match binary.op {
                BinaryOperator::Add => Instruction::Add(result_var, left, right),
                BinaryOperator::Sub => Instruction::Sub(result_var, left, right),
                BinaryOperator::Mul => Instruction::Mul(result_var, left, right),
                BinaryOperator::Div => Instruction::Div(result_var, left, right),
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
                _ => todo!()
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
    use crate::ast::expressions::binary::BinaryOperator;
    use crate::ast::expressions::Expression;
    use crate::ast::typing::{typecheck_expression};
    use crate::ir::function::{Block, Function};
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use parameterized::ide;
    use paste::paste;
    use crate::errors::Errors;
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
