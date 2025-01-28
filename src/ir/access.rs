use crate::ir::function::Instruction;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::scope::ir::IrVarScope;
use assert_matches::assert_matches;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::expressions::access::AccessExpression;

// TODO: all generate ir functions should take their expression type directly rather than an Expression
pub(super) fn generate_ir_for_access_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: VarId,
    expr: &Expression,
) {
    let access = assert_matches!(&expr.kind, ExpressionKind::Access(x) => x);
    let var = get_access_var(ir, scope, access);
    ir.instr(Instruction::Assign(target, var));
}

pub(super) fn get_access_var(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    access: &AccessExpression,
) -> VarId {
    *scope.lookup(&access.path).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::path::Path;
    use crate::ir::function::Block;
    use crate::ir::ids::TypeId;
    use crate::ir::test::IrTestEnv;
    use ustr::ustr;
    use crate::types::type_ref::{BuiltinType, TypeRef};

    #[test]
    fn access_local() {
        // arrange
        let mut env = IrTestEnv::new();
        let mut expr = Expression::access(0, Path::name("foo"));

        let foo_var = env.function_ir_builder.create_local(TypeId::i32());
        let out_var = env.function_ir_builder.create_local(TypeId::i32());

        env.ir_var_scope.insert(ustr("foo"), foo_var);
        env.type_scope.insert(ustr("foo"), TypeRef::Builtin(BuiltinType::F32));

        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_access_expr(&mut env.function_ir_builder, &env.ir_var_scope, out_var, &expr);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 2);
        assert_eq!(
            func.blocks,
            vec![Block {
                instructions: vec![Instruction::Assign(out_var, foo_var),]
            }]
        );
    }
}
