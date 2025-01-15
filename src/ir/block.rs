use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::statement::Statement;
use crate::ir::function::generate_ir_for_expr;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::scope::ir::IrVarScope;
use assert_matches::assert_matches;

pub(super) fn generate_block_for_statement(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    stmt: &Statement,
) {
    match stmt {
        Statement::Expression(expr) => {
            generate_ir_for_expr(ir, scope, None, expr);
        }
    }
}

pub(super) fn generate_ir_for_block_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: Option<VarId>,
    expr: &Expression,
) {
    let block = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

    let new_scope = match &block.let_ {
        Some(decl) => {
            let type_id = ir.map_type(decl.type_ref.as_ref().unwrap());
            let var = ir.create_local(type_id);
            if let Some(value) = &decl.value {
                generate_ir_for_expr(ir, scope, Some(var), value);
            }
            let mut new_scope = scope.nested();
            new_scope.insert(decl.binding, var);
            Some(new_scope)
        }
        _ => {
            None
        }
    };

    let scope = new_scope.as_ref().unwrap_or(scope);

    for stmt in &block.statements {
        generate_block_for_statement(ir, scope, stmt);
    }

    if let Some(value) = &block.value {
        generate_ir_for_expr(ir, scope, target, value);
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::Expression;
    use crate::ast::path::Path;
    use crate::ast::statement::Statement;
    use crate::ir::block::generate_ir_for_block_expr;
    use crate::ir::function::{Block, Instruction};
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::test::IrTestEnv;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use ustr::ustr;

    #[test]
    fn empty() {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(0, vec![], None);
        env.typecheck_expression(&mut expr);
        // act
        generate_ir_for_block_expr(&mut env.function_ir_builder, &env.ir_var_scope, None, &expr);

        // assert
        let func = env.get_function();
        assert!(func.locals.is_empty());
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![],
            }]
        );
    }
    #[test]
    fn just_value() {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(0, vec![], Some(Expression::int_literal(0, 1)));
        env.typecheck_expression(&mut expr);

        let result_var = env.function_ir_builder.create_local(TypeId::i32());

        // act
        generate_ir_for_block_expr(&mut env.function_ir_builder, &env.ir_var_scope, Some(result_var), &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_matches!(&func.blocks[..], [
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
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::int_literal(0, 1)),
                Statement::Expression(Expression::int_literal(0, 2)),
            ],
            None,
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_block_expr(&mut env.function_ir_builder, &env.ir_var_scope, None, &expr);

        // assert
        let func = env.get_function();
        assert!(func.locals.is_empty());
        assert_matches!(&func.blocks[..], [
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
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::int_literal(0, 1)),
                Statement::Expression(Expression::int_literal(0, 2)),
            ],
            Some(Expression::int_literal(0, 3)),
        );
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::i32());

        // act
        generate_ir_for_block_expr(&mut env.function_ir_builder, &env.ir_var_scope, Some(result_var), &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_matches!(&func.blocks[..], [
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
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1))),
            vec![],
            Some(Expression::access(0, Path::name("foo"))),
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_block_expr(&mut env.function_ir_builder, &env.ir_var_scope, None, &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        assert_matches!(&func.blocks[..], [
            Block {
                instructions
            }
        ] => {
            assert_matches!(&instructions[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Assign(v3, v2),
            ] => {
                assert_eq!(v1, v2);
                assert_eq!(v3, &VarId::discard());
            })
        })
    }
}
