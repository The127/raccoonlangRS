use crate::ast::expressions::{Expression, ExpressionKind};
use crate::errors::Errors;
use crate::ir::function::{generate_ir_for_expr_as_var, Instruction};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::scope::ir::IrVarScope;
use assert_matches::assert_matches;

pub(super) fn generate_ir_for_tuple_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: VarId,
    expr: &Expression,
    errors: &mut Errors,
) {
    let tuple = assert_matches!(&expr.kind, ExpressionKind::Tuple(x) => x);

    let tuple_members = tuple
        .values
        .iter()
        .map(|e| generate_ir_for_expr_as_var(ir, scope, e, errors))
        .collect();

    ir.instr(Instruction::Tuple(target, tuple_members));
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::types::type_ref::{BuiltinType, TypeRef};
    use crate::errors::Errors;
    use crate::ir::function::Instruction;
    use crate::ir::test::IrTestEnv;
    use crate::ir::tuple::generate_ir_for_tuple_expr;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;

    #[test]
    fn tuple() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::tuple(
            0,
            vec![Expression::i32_literal(0, 1), Expression::i32_literal(0, 2)],
        );
        env.typecheck_expression(&mut expr);
        let tuple_type = env.function_ir_builder.map_type(&TypeRef::tuple(vec![
            TypeRef::Builtin(BuiltinType::I32),
            TypeRef::Builtin(BuiltinType::I32),
        ]));
        let target = env.function_ir_builder.create_local(tuple_type);

        // act
        generate_ir_for_tuple_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            target,
            &expr,
            &mut errors,
        );

        // assert
        let func = env.get_function();
        assert_matches!(
            &func.blocks[0].instructions[..],
            [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Const(v2, ConstantValue::I32(2)),
                Instruction::Tuple(v3, tuple_vars),
            ] => {
                assert_eq!(v3, &target);
                assert_eq!(tuple_vars, &vec![*v1, *v2]);
            }
        );
    }
}
