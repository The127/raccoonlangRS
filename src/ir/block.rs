use assert_matches::assert_matches;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::statement::Statement;
use crate::ir::function::generate_ir_for_expr;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;

pub(super) fn generate_block_for_statement(ir: &mut FunctionIrBuilder, stmt: &Statement) {
    match stmt {
        Statement::Expression(expr) => {
            generate_ir_for_expr(ir, expr);
        }
    }
}

pub(super) fn generate_ir_for_block_expr(
    ir: &mut FunctionIrBuilder,
    expr: &Expression,
) -> Option<VarId> {
    let block = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

    if let Some(decl) = &block.let_ {
        let var = ir.create_local(ir.map_type(decl.type_ref.as_ref().unwrap()));
        if let Some(value) = &decl.value {
            generate_ir_for_expr(ir, value);
            // TODO: need to tell it to store the result in the var we just created!
        }
    }

    for stmt in &block.statements {
        generate_block_for_statement(ir, stmt);
    }

    match &block.value {
        Some(result) => generate_ir_for_expr(ir, result),
        None => None,
    }
}


#[cfg(test)]
mod test {
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::Expression;
    use crate::ast::scope::global::GlobalScope;
    use crate::ast::statement::Statement;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::block::generate_ir_for_block_expr;
    use crate::ir::function::{Block, Function, Instruction};
    use crate::ir::function_ir_builder::FunctionIrBuilder;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use ustr::ustr;

    #[test]
    fn empty() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

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
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

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
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

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
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

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

    #[test]
    fn just_decl() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1))),
            vec![],
            None,
        );
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        // act
        let var_id = generate_ir_for_block_expr(&mut ir, &expr);

        // assert
        assert_eq!(var_id, None);
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
            ])
        })
    }

    // TODO: generate IR for let decl of block
}
