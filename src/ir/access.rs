use crate::ast::expressions::Expression;
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;

pub(super) fn generate_ir_for_access_expr(ir: &mut FunctionIrBuilder, expr: &Expression) -> VarId {
    ir.create_local(ir.map_type(expr.type_ref.as_ref().unwrap()))
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::Expression;
    use crate::scope::type_::TypeScope;
    use crate::ast::typing::typecheck_expression;
    use crate::errors::Errors;
    use crate::ir::function::{generate_ir_for_expr, Function};
    use crate::ir::function_ir_builder::FunctionIrBuilder;
    use crate::ir::literal::generate_ir_for_literal_expr;
    use ustr::ustr;

    #[test]
    fn access_block_local() {
        // arrange
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 123))),
            vec![],
            Some(Expression::access(0, ustr("foo"))),
        );
        let mut function = Function::new();
        let mut ir = FunctionIrBuilder::new(&mut function);
        let mut errors = Errors::new();
        let scope = TypeScope::new();
        typecheck_expression(&mut expr, &scope, &mut errors);
        assert!(errors.get_errors().is_empty());

        // act
        let var_id = generate_ir_for_expr(&mut ir, &expr);

        // assert
        assert_eq!(function.locals.len(), 1);
        // TODO: assert that the access expression refers to the correct local

    }
}
