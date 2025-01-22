use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ir::function::{generate_ir_for_expr, Instruction};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::{TypeId, VarId};
use crate::scope::ir::IrVarScope;
use assert_matches::assert_matches;
use crate::errors::Errors;

pub(super) fn generate_ir_for_if_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: Option<VarId>,
    expr: &Expression,
    errors: &mut Errors,
) {
    let if_expr = assert_matches!(&expr.kind, ExpressionKind::If(x) => x);

    let cond_var = ir.create_local(TypeId::bool());
    generate_ir_for_expr(ir, scope, Some(cond_var), if_expr.condition.as_ref(), errors);

    let then_block = ir.create_block();

    let else_block = match if_expr.else_ {
        Some(_) => Some(ir.create_block()),
        None => None,
    };

    let final_block = if let Some(target_var) = target {
        ir.create_block_with_params(vec![target_var])
    } else {
        ir.create_block()
    };

    ir.instr(Instruction::BranchIf(
        cond_var,
        then_block,
        else_block.unwrap_or(final_block),
    ));

    ir.set_block(then_block);
    generate_ir_for_expr(ir, scope, target, if_expr.then.as_ref(), errors);
    ir.instr(Instruction::Branch(final_block));

    if let Some(else_expr) = &if_expr.else_ {
        ir.set_block(else_block.unwrap());
        generate_ir_for_expr(ir, scope, target, else_expr, errors);
        ir.instr(Instruction::Branch(final_block));
    }

    ir.set_block(final_block);
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::statement::Statement;
    use crate::ir::function::{Block, Instruction};
    use crate::ir::function_ir_builder::BlockId;
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::if_::generate_ir_for_if_expr;
    use crate::ir::test::IrTestEnv;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use crate::errors::Errors;

    #[test]
    fn no_else_no_value() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![Statement::Expression(Expression::i32_literal(0, 123))],
                None,
            ),
            None,
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_if_expr(&mut env.function_ir_builder, &env.ir_var_scope, None, &expr, &mut errors);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);

        assert_matches!(&func.blocks[..], [
            Block {
                instructions: i0,
            },
            Block {
                instructions: i1,
            },
            Block {
                instructions: i2,
            },
        ] => {
            assert_matches!(&i0[..], [
                Instruction::Const(v1a, ConstantValue::Bool(true)),
                Instruction::BranchIf(v1b, BlockId(1), BlockId(2)),
            ] => {
                assert_eq!(v1a, v1b);
            });
            assert_matches!(&i1[..], [
                Instruction::Const(discard, ConstantValue::I32(123)),
                Instruction::Branch(BlockId(2))
            ] => {
                assert_eq!(discard, &VarId::discard());
            });
            assert!(i2.is_empty());
        });
    }

    #[test]
    fn else_no_value() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![Statement::Expression(Expression::i32_literal(0, 1))],
                None,
            ),
            Some(Expression::block(
                0,
                vec![Statement::Expression(Expression::i32_literal(0, 2))],
                None,
            )),
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_if_expr(&mut env.function_ir_builder, &env.ir_var_scope, None, &expr, &mut errors);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);

        assert_matches!(&func.blocks[..], [
            Block {
                instructions: i0,
            },
            Block {
                instructions: i1,
            },
            Block {
                instructions: i2,
            },
            Block {
                instructions: i3
            },
        ] => {
            assert_matches!(&i0[..], [
                Instruction::Const(v1a, ConstantValue::Bool(true)),
                Instruction::BranchIf(v1b, BlockId(1),BlockId(2)),
            ] => {
                assert_eq!(v1a, v1b);
            });
            assert_matches!(&i1[..], [
                Instruction::Const(discard, ConstantValue::I32(1)),
                Instruction::Branch(BlockId(3))
            ] => {
                assert_eq!(discard, &VarId::discard());
            });
            assert_matches!(&i2[..], [
                Instruction::Const(discard, ConstantValue::I32(2)),
                Instruction::Branch(BlockId(3))
            ] => {
                assert_eq!(discard, &VarId::discard());
            });

            assert!(i3.is_empty());
        });
    }

    #[test]
    fn else_value() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(0, vec![], Some(Expression::i32_literal(0, 1))),
            Some(Expression::block(
                0,
                vec![],
                Some(Expression::i32_literal(0, 2)),
            )),
        );
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::i32());

        // act
        generate_ir_for_if_expr(&mut env.function_ir_builder, &env.ir_var_scope, Some(result_var), &expr, &mut errors);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 2);

        assert_matches!(&func.blocks[..], [
            Block {
                instructions: i0,
            },
            Block {
                instructions: i1,
            },
            Block {
                instructions: i2,
            },
            Block {
                instructions: i3
            },
        ] => {
            assert_matches!(&i0[..], [
                Instruction::Const(v1a, ConstantValue::Bool(true)),
                Instruction::BranchIf(v1b, BlockId(1) ,BlockId(2)),
            ] => {
                assert_eq!(v1a, v1b);
            });
            assert_matches!(&i1[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Branch(BlockId(3))
            ] => {
                assert_eq!(v1, &result_var);
            });
            assert_matches!(&i2[..], [
                Instruction::Const(v1, ConstantValue::I32(2)),
                Instruction::Branch(BlockId(3))
            ] => {
                assert_eq!(v1, &result_var);
            });

            assert!(i3.is_empty());
        });
    }
}
