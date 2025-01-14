use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::statement::Statement;
use crate::ir::function::generate_ir_for_expr;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use assert_matches::assert_matches;

pub(super) fn generate_block_for_statement(ir: &mut FunctionIrBuilder, stmt: &Statement) {
    match stmt {
        Statement::Expression(expr) => {
            generate_ir_for_expr(ir, None, expr);
        }
    }
}

pub(super) fn generate_ir_for_block_expr(
    ir: &mut FunctionIrBuilder,
    target: Option<VarId>,
    expr: &Expression,
) {
    let block = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

    if let Some(decl) = &block.let_ {
        // TODO: somehow associate this var with the decl
        let var = ir.create_local(ir.map_type(decl.type_ref.as_ref().unwrap()));
        if let Some(value) = &decl.value {
            generate_ir_for_expr(ir, Some(var), value);
        }
    }

    for stmt in &block.statements {
        generate_block_for_statement(ir, stmt);
    }

    if let Some(value) = &block.value {
        generate_ir_for_expr(ir, target, value);
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::Expression;
    use crate::scope::type_::TypeScope;
    use crate::ast::statement::Statement;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::block::generate_ir_for_block_expr;
    use crate::ir::function::{Block, Function, Instruction};
    use crate::ir::function_ir_builder::FunctionIrBuilder;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use ustr::ustr;
    use crate::ir::ids::{TypeId, VarId};

    #[test]
    fn empty() {
        // arrange
        let mut expr = Expression::block(0, vec![], None);
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        // act
        generate_ir_for_block_expr(&mut ir, None, &expr);

        // assert
        assert!(function.locals.is_empty());
        assert_eq!(
            function.blocks,
            vec![Block {
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
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let result_var = ir.create_local(TypeId::i32());

        // act
        generate_ir_for_block_expr(&mut ir, Some(result_var), &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        assert_matches!(&function.blocks[..], [
            Block {
                instructions
            }
        ] => {
            assert_matches!(&instructions[..], [
                Instruction::Const(var, ConstantValue::I32(1))
            ] => {
                assert_eq!(var, &result_var);
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
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        // act
        generate_ir_for_block_expr(&mut ir, None, &expr);

        // assert
        assert!(function.locals.is_empty());
        assert_matches!(&function.blocks[..], [
            Block {
                instructions
            }
        ] => {
            assert_matches!(&instructions[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Const(v2, ConstantValue::I32(2)),
            ] => {
                assert_eq!(v1, &VarId::discard());
                assert_eq!(v2, &VarId::discard());
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
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let result_var = ir.create_local(TypeId::i32());

        // act
        generate_ir_for_block_expr(&mut ir, Some(result_var), &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        assert_matches!(&function.blocks[..], [
            Block {
                instructions
            }
        ] => {
            assert_matches!(&instructions[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Const(v2, ConstantValue::I32(2)),
                Instruction::Const(v3, ConstantValue::I32(3))
            ] => {
                assert_eq!(v1, &VarId::discard());
                assert_eq!(v2, &VarId::discard());
                assert_eq!(v3, &result_var);
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
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        // act
        generate_ir_for_block_expr(&mut ir, None, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        assert_matches!(&function.blocks[..], [
            Block {
                instructions
            }
        ] => {
            assert_matches!(&instructions[..], [
                Instruction::Const(var, ConstantValue::I32(1))
            ])
        })
    }

    // TODO: generate IR for let decl of block
}
