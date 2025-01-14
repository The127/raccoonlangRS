use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::typing::{BuiltinType, TypeRef};
use crate::ir::function::{generate_ir_for_expr, Instruction};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::{TypeId, VarId};
use assert_matches::assert_matches;
use crate::scope::ir::IrVarScope;

pub(super) fn generate_ir_for_if_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: Option<VarId>,
    expr: &Expression,
) {
    let if_expr = assert_matches!(&expr.kind, ExpressionKind::If(x) => x);

    let cond_var = ir.create_local(TypeId::bool());
    generate_ir_for_expr(ir, scope, Some(cond_var), if_expr.condition.as_ref());

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
    generate_ir_for_expr(ir, scope, target, if_expr.then.as_ref());
    ir.instr(Instruction::Branch(final_block));

    if let Some(else_expr) = &if_expr.else_ {
        ir.set_block(else_block.unwrap());
        generate_ir_for_expr(ir, scope, target, else_expr);
        ir.instr(Instruction::Branch(final_block));
    }

    ir.set_block(final_block);
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::statement::Statement;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::function::{Block, Function, Instruction};
    use crate::ir::function_ir_builder::{BlockId, FunctionIrBuilder};
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::if_::generate_ir_for_if_expr;
    use crate::ir::ConstantValue;
    use crate::scope::type_::TypeScope;
    use assert_matches::assert_matches;
    use crate::scope::ir::IrVarScope;

    #[test]
    fn no_else_no_value() {
        // arrange
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![Statement::Expression(Expression::int_literal(0, 123))],
                None,
            ),
            None,
        );
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let scope = IrVarScope::new();

        // act
        generate_ir_for_if_expr(&mut ir, &scope, None, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);

        assert_matches!(&function.blocks[..], [
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
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![Statement::Expression(Expression::int_literal(0, 1))],
                None,
            ),
            Some(Expression::block(
                0,
                vec![Statement::Expression(Expression::int_literal(0, 2))],
                None,
            )),
        );
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let scope = IrVarScope::new();

        // act
        generate_ir_for_if_expr(&mut ir, &scope, None, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);

        assert_matches!(&function.blocks[..], [
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
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(0, vec![], Some(Expression::int_literal(0, 1))),
            Some(Expression::block(
                0,
                vec![],
                Some(Expression::int_literal(0, 2)),
            )),
        );
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let result_var = ir.create_local(TypeId::i32());
        let scope = IrVarScope::new();

        // act
        generate_ir_for_if_expr(&mut ir, &scope, Some(result_var), &expr);

        // assert
        assert_eq!(function.locals.len(), 2);

        assert_matches!(&function.blocks[..], [
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
