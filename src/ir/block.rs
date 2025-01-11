use crate::ast::expressions::{BlockExpression, Expression, ExpressionKind};
use crate::ast::statement::Statement;
use crate::ir::function::generate_ir_for_expr;
use crate::ir::ids::VarId;
use crate::ir::ir_builder::IrBuilder;

pub(super) fn generate_block_for_statement(ir: &mut IrBuilder, stmt: &Statement) {
    match stmt {
        Statement::Expression(expr) => {
            generate_ir_for_expr(ir, expr);
        }
        Statement::Declaration(_) => todo!(),
    }
}

pub(super) fn generate_ir_for_block_expr(ir: &mut IrBuilder, expr: &Expression) -> Option<VarId> {
    match &expr.kind {
        ExpressionKind::Block(block) => {
            for stmt in &block.statements {
                generate_block_for_statement(ir, stmt);
            }

            match &block.value {
                Some(result) => generate_ir_for_expr(ir, result),
                None => None,
            }
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::statement::Statement;
    use crate::ast::typing::{calculate_expression_type, Scope};
    use crate::ir::block::generate_ir_for_block_expr;
    use crate::ir::function::{Block, Function, Instruction};
    use crate::ir::ir_builder::IrBuilder;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;

    #[test]
    fn empty() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_block_expr(&mut ir, &expr);

        // assert
        assert_eq!(var_id, None);
        assert!(function.locals.is_empty());
        assert_eq!(
            function.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![],
            }]
        );
    }
    #[test]
    fn just_value() {
        // arrange
        let mut expr = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_block_expr(&mut ir, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        assert_matches!(&function.blocks[..], [
            Block {
                params,
                instructions
            }
        ] => {
            assert!(params.is_empty());
            assert_matches!(&instructions[..], [
                Instruction::Const(var, ConstantValue::I32(1))
            ] => {
                assert_eq!(var_id, Some(*var));
            })
        })
    }

    #[test]
    fn just_statements() {
        // arrange
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::int_literal(0, 1)),
                Statement::Expression(Expression::int_literal(0, 2)),
            ],
            None,
        );
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_block_expr(&mut ir, &expr);

        // assert
        assert_eq!(var_id, None);
        assert_eq!(function.locals.len(), 2);
        assert_matches!(&function.blocks[..], [
            Block {
                params,
                instructions
            }
        ] => {
            assert!(params.is_empty());
            assert_matches!(&instructions[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Const(v2, ConstantValue::I32(2)),
            ] => {
                assert_ne!(v1, v2);
            })
        })
    }

    #[test]
    fn statements_and_value() {
        // arrange
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::int_literal(0, 1)),
                Statement::Expression(Expression::int_literal(0, 2)),
            ],
            Some(Expression::int_literal(0, 3)),
        );
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_block_expr(&mut ir, &expr);

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
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Const(v2, ConstantValue::I32(2)),
                Instruction::Const(v3, ConstantValue::I32(3))
            ] => {
                assert_ne!(v1, v2);
                assert_eq!(var_id, Some(*v3))
            })
        })
    }
}
