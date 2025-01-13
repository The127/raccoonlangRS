use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::expressions::literal::{LiteralExpression, LiteralValue};
use crate::ir::function::Instruction;
use crate::ir::ids::{TypeId, VarId};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ConstantValue;

pub(super) fn generate_ir_for_literal_expr(ir: &mut FunctionIrBuilder, expr: &Expression) -> VarId {
    let result_type = ir.map_type(expr.type_ref.as_ref().unwrap());
    let result = ir.create_local(result_type);

    match &expr.kind {
        ExpressionKind::Literal(LiteralExpression {value, .. }) => {
            match value {
                LiteralValue::Integer(val) => {
                    assert_eq!(result_type, TypeId::i32());
                    ir.instr(Instruction::Const(result, ConstantValue::I32(*val)));
                }
                LiteralValue::Boolean(val) => {
                    assert_eq!(result_type, TypeId::bool());
                    ir.instr(Instruction::Const(result, ConstantValue::Bool(*val)));
                }
            }
        },
        _ => unreachable!(),
    }
    result
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ir::function::{Block, Function};
    use parameterized::{ide, parameterized};
    use crate::ast::scope::global::GlobalScope;
    use crate::ast::typing::{typecheck_expression};
    use crate::errors::Errors;

    ide!();
    #[parameterized(value = {-5, 0, 1, 1024})]
    fn int_literal(value: i32) {
        // arrange
        let mut expr = Expression::int_literal(0, value);
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        // act
        let var_id = generate_ir_for_literal_expr(&mut ir, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        let (v1, t1) = function.locals[0];
        assert_eq!(t1, TypeId::i32());
        assert_eq!(v1, var_id);
        assert_eq!(
            function.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![Instruction::Const(v1, ConstantValue::I32(value)),]
            }]
        );
    }

    #[parameterized(value = {true, false})]
    fn bool_literal(value: bool) {
        // arrange
        let mut expr = Expression::bool_literal(0, value);
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = GlobalScope::new();

        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        // act
        let var_id = generate_ir_for_literal_expr(&mut ir, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        let (v1, t1) = function.locals[0];
        assert_eq!(t1, TypeId::bool());
        assert_eq!(v1, var_id);
        assert_eq!(
            function.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![Instruction::Const(v1, ConstantValue::Bool(value)),]
            }]
        );
    }
}
