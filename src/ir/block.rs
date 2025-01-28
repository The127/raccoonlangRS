use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::pattern::Pattern;
use crate::ast::statement::Statement;
use crate::types::type_ref::TypeRef;
use crate::errors::Errors;
use crate::ir::function::{generate_ir_for_expr, generate_ir_for_expr_as_var, Instruction};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::VarId;
use crate::scope::ir::IrVarScope;
use crate::scope::Scope;
use assert_matches::assert_matches;

pub(super) fn generate_block_for_statement(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    stmt: &Statement,
    errors: &mut Errors,
) {
    match stmt {
        Statement::Expression(expr) => {
            generate_ir_for_expr(ir, scope, None, expr, errors);
        }
    }
}

pub(super) fn generate_ir_for_block_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: Option<VarId>,
    expr: &Expression,
    errors: &mut Errors,
) {
    let block = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

    let new_scope = match &block.let_ {
        Some(decl) => {
            fn handle_pattern(
                ir: &mut FunctionIrBuilder,
                scope: &mut Scope<VarId>,
                pattern: &Pattern,
                type_ref: &TypeRef,
                var: VarId,
            ) {
                match pattern {
                    Pattern::Discard => {}
                    Pattern::Name(name) => {
                        scope.insert(*name, var);
                    }
                    Pattern::Tuple(sub_patterns) => {
                        let tuple_type_ref = assert_matches!(type_ref, TypeRef::Tuple(x) => x);
                        for (idx, (pattern, type_ref)) in sub_patterns
                            .iter()
                            .zip(tuple_type_ref.fields.iter())
                            .enumerate()
                        {
                            if pattern != &Pattern::Discard {
                                let type_id = ir.map_type(type_ref);
                                let field_var = ir.create_local(type_id);
                                ir.instr(Instruction::TupleAccess(field_var, var, idx));
                                handle_pattern(ir, scope, pattern, type_ref, field_var);
                            }
                        }
                    }
                }
            }

            let mut new_scope = scope.nested();

            if decl.binding == Pattern::Discard {
                generate_ir_for_expr(ir, scope, None, &decl.value, errors);
            } else {
                let var = generate_ir_for_expr_as_var(ir, scope, &decl.value, errors);
                handle_pattern(
                    ir,
                    &mut new_scope,
                    &decl.binding,
                    decl.type_ref.as_ref().unwrap(),
                    var,
                );
            }

            Some(new_scope)
        }
        _ => None,
    };

    let scope = new_scope.as_ref().unwrap_or(scope);

    for stmt in &block.statements {
        generate_block_for_statement(ir, scope, stmt, errors);
    }

    if let Some(value) = &block.value {
        generate_ir_for_expr(ir, scope, target, value, errors);
    }
}

#[cfg(test)]
mod test {
    use crate::ast::path::Path;
    use crate::ast::pattern::Pattern;
    use crate::ast::statement::Statement;
    use crate::types::type_ref::{BuiltinType, TupleType, TypeRef};
    use crate::errors::Errors;
    use crate::ir::block::generate_ir_for_block_expr;
    use crate::ir::function::{Block, Instruction};
    use crate::ir::ids::{TypeId, VarId};
    use crate::ir::test::IrTestEnv;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;
    use ustr::ustr;
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::Expression;

    #[test]
    fn empty() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(0, vec![], None);
        env.typecheck_expression(&mut expr);
        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            None,
            &expr,
            &mut errors,
        );

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
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(0, vec![], Some(Expression::i32_literal(0, 1)));
        env.typecheck_expression(&mut expr);

        let result_var = env.function_ir_builder.create_local(TypeId::i32());

        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            Some(result_var),
            &expr,
            &mut errors,
        );

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
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::i32_literal(0, 1)),
                Statement::Expression(Expression::i32_literal(0, 2)),
            ],
            None,
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            None,
            &expr,
            &mut errors,
        );

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
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::i32_literal(0, 1)),
                Statement::Expression(Expression::i32_literal(0, 2)),
            ],
            Some(Expression::i32_literal(0, 3)),
        );
        env.typecheck_expression(&mut expr);
        let result_var = env.function_ir_builder.create_local(TypeId::i32());

        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            Some(result_var),
            &expr,
            &mut errors,
        );

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
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, Pattern::Name(ustr("foo")), Expression::i32_literal(0, 1)),
            vec![],
            Some(Expression::access(0, Path::name("foo"))),
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            None,
            &expr,
            &mut errors,
        );

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
        });
    }

    #[test]
    fn decl_with_discard() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, Pattern::Discard, Expression::i32_literal(0, 1)),
            vec![],
            None,
        );
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            None,
            &expr,
            &mut errors,
        );

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
            ] => {
                assert_eq!(v1, &VarId::discard());
            })
        });
    }

    #[test]
    fn decl_with_tuple() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(
                0,
                Pattern::Tuple(vec![
                    Pattern::Tuple(vec![
                        Pattern::Name(ustr("x")),
                        Pattern::Name(ustr("y")),
                        Pattern::Discard,
                    ]),
                    Pattern::Discard,
                    Pattern::Name(ustr("foo")),
                ]),
                Expression::access(0, Path::name("asdf")),
            ),
            vec![],
            Some(Expression::tuple(
                0,
                vec![
                    Expression::access(0, Path::name("foo")),
                    Expression::access(0, Path::name("x")),
                    Expression::access(0, Path::name("y")),
                ],
            )),
        );
        env.type_scope.insert(
            ustr("asdf"),
            TypeRef::Tuple(TupleType {
                fields: vec![
                    TypeRef::Tuple(TupleType {
                        fields: vec![
                            TypeRef::Builtin(BuiltinType::I32),
                            TypeRef::Builtin(BuiltinType::I32),
                            TypeRef::Builtin(BuiltinType::I32),
                        ],
                    }),
                    TypeRef::Builtin(BuiltinType::I32),
                    TypeRef::Builtin(BuiltinType::Bool),
                ],
            }),
        );
        env.ir_var_scope.insert(ustr("asdf"), VarId::param(1));
        env.typecheck_expression(&mut expr);

        // act
        generate_ir_for_block_expr(
            &mut env.function_ir_builder,
            &env.ir_var_scope,
            None,
            &expr,
            &mut errors,
        );

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 4);
        assert_matches!(&func.blocks[..], [
            Block {
                instructions
            }
        ] => {
            assert_matches!(&instructions[..], [
                Instruction::TupleAccess(v1a, pa, 0),
                Instruction::TupleAccess(v2, v1b, 0),
                Instruction::TupleAccess(v3, v1c, 1),
                Instruction::TupleAccess(v4, pb, 2),
                Instruction::Tuple(d, tuple_params),
            ] => {
                assert_eq!(pa, &VarId::param(1));
                assert_eq!(pb, &VarId::param(1));
                assert_eq!(v1a, v1b);
                assert_eq!(v1a, v1c);
                assert_eq!(tuple_params, &vec![*v4, *v2, *v3]);
                assert_eq!(d, &VarId::discard());
            })
        });
    }
}
