use assert_matches::assert_matches;
use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ir::function::Instruction;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::scope::ir::IrVarScope;


// TODO: all generate ir functions should take their expression type directly rather than an Expression
pub(super) fn generate_ir_for_access_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: VarId,
    expr: &Expression,
) {
    let access = assert_matches!(&expr.kind, ExpressionKind::Access(x) => x);
    let var = scope.lookup(&access.path).unwrap();
    ir.instr(Instruction::Assign(target, *var));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::scope::type_::TypeScope;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::function::{Block, Function};
    use crate::ir::function_ir_builder::FunctionIrBuilder;
    use ustr::ustr;
    use crate::ast::path::Path;
    use crate::ir::ids::TypeId;

    #[test]
    fn access_local() {
        // arrange
        let mut expr = Expression::access(0, Path::name("foo"));
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        let foo_var = ir.create_local(TypeId::i32());
        let out_var = ir.create_local(TypeId::i32());
        let scope = IrVarScope::from(&[(ustr("foo"), foo_var)]);

        // act
        generate_ir_for_access_expr(&mut ir, &scope, out_var, &expr);

        // assert
        assert_eq!(function.locals.len(), 2);
        assert_eq!(function.blocks, vec![Block {
            instructions: vec![
                Instruction::Assign(out_var, foo_var),
            ]
        }]);
    }
}
