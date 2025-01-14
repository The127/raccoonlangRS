use crate::ast::expressions::literal::{LiteralExpression, LiteralValue};
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ir::function::Instruction;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::{TypeId, VarId};
use crate::ir::ConstantValue;
use assert_matches::assert_matches;

pub(super) fn generate_ir_for_literal_expr(
    ir: &mut FunctionIrBuilder,
    target: VarId,
    expr: &Expression,
) {
    let literal = assert_matches!(&expr.kind, ExpressionKind::Literal(x) => x);
    match &literal.value {
        LiteralValue::Integer(val) => {
            ir.instr(Instruction::Const(target, ConstantValue::I32(*val)));
        }
        LiteralValue::Boolean(val) => {
            ir.instr(Instruction::Const(target, ConstantValue::Bool(*val)));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::function::{Block, Function};
    use crate::scope::type_::TypeScope;
    use parameterized::{ide, parameterized};

    ide!();
    #[parameterized(value = {-5, 0, 1, 1024})]
    fn int_literal(value: i32) {
        // arrange
        let mut expr = Expression::int_literal(0, value);
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let result_var = ir.create_local(TypeId::i32());

        // act
        generate_ir_for_literal_expr(&mut ir, result_var, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        assert_eq!(
            function.blocks,
            vec![Block {
                instructions: vec![Instruction::Const(result_var, ConstantValue::I32(value)),]
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
        let scope = TypeScope::new();

        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());
        let result_var = ir.create_local(TypeId::bool());

        // act
        generate_ir_for_literal_expr(&mut ir, result_var, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        assert_eq!(
            function.blocks,
            vec![Block {
                instructions: vec![Instruction::Const(result_var, ConstantValue::Bool(value)),]
            }]
        );
    }
}
