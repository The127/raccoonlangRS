use crate::ast::expressions::{Expression, ExpressionKind};
use crate::ast::typing::{BuiltinType, TypeRef};
use crate::ir::function::{generate_ir_for_expr, BranchTarget, Instruction};
use crate::ir::ids::{TypeId, VarId};
use crate::ir::ir_builder::IrBuilder;
use assert_matches::assert_matches;

pub(super) fn generate_ir_for_if_expr(ir: &mut IrBuilder, expr: &Expression) -> Option<VarId> {
    let if_expr = assert_matches!(&expr.kind, ExpressionKind::If(x) => x);

    let has_result = !matches!(expr.type_ref, Some(TypeRef::Builtin(BuiltinType::Unit)));

    let cond_var = generate_ir_for_expr(ir, if_expr.condition.as_ref()).unwrap();

    let then_block = ir.create_block();

    let else_block = match if_expr.else_ {
        Some(_) => Some(ir.create_block()),
        None => None,
    };

    let (result_var, final_block) = if has_result {
        let result_var = ir.create_local(ir.map_type(expr.type_ref.as_ref().unwrap()));
        (Some(result_var), ir.create_block_with_params(vec![result_var]))
    } else {
        (None, ir.create_block())
    };

    let else_target = match else_block {
        Some(b) => BranchTarget::new(b),
        _ => BranchTarget::new(final_block),
    };

    ir.instr(Instruction::BranchIf(
        cond_var,
        BranchTarget::new(then_block),
        else_target,
    ));

    ir.set_block(then_block);
    let then_value = generate_ir_for_expr(ir, if_expr.then.as_ref());
    let then_branch_target = if has_result {
        BranchTarget::with_args(final_block, vec![then_value.unwrap()])
    } else {
        BranchTarget::new(final_block)
    };
    ir.instr(Instruction::Branch(then_branch_target));

    if let Some(else_expr) = &if_expr.else_ {
        ir.set_block(else_block.unwrap());
        let else_value = generate_ir_for_expr(ir, else_expr);
        let else_branch_target = if has_result {
            BranchTarget::with_args(final_block, vec![else_value.unwrap()])
        } else {
            BranchTarget::new(final_block)
        };
        ir.instr(Instruction::Branch(else_branch_target));
    }

    ir.set_block(final_block);

    result_var
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::statement::Statement;
    use crate::ast::typing::{calculate_expression_type, Scope};
    use crate::ir::function::{Block, BranchTarget, Function, Instruction};
    use crate::ir::ids::TypeId;
    use crate::ir::if_::generate_ir_for_if_expr;
    use crate::ir::ir_builder::{BlockId, IrBuilder};
    use crate::ir::literal::generate_ir_for_literal_expr;
    use crate::ir::ConstantValue;
    use assert_matches::assert_matches;

    #[test]
    fn no_else_no_value() {
        // arrange
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![Statement::Expression(Expression::int_literal(0, 123))],
                None,
            ),
            None,
        );
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_if_expr(&mut ir, &expr);

        // assert
        assert_eq!(var_id, None);
        assert_eq!(function.locals.len(), 2);

        assert_matches!(&function.blocks[..], [
            Block {
                params: p0,
                instructions: i0,
            },
            Block {
                params: p1,
                instructions: i1,
            },
            Block {
                params: p2,
                instructions: i2,
            },
        ] => {
            assert!(p0.is_empty());
            assert!(p1.is_empty());
            assert!(p2.is_empty());

            assert_matches!(&i0[..], [
                Instruction::Const(v1a, ConstantValue::Bool(true)),
                Instruction::BranchIf(v1b, BranchTarget {
                    block_index: BlockId(1),
                    arguments: a1,
                }, BranchTarget {
                    block_index: BlockId(2),
                    arguments: a2
                }),
            ] => {
                assert_eq!(v1a, v1b);
                assert!(a1.is_empty());
                assert!(a2.is_empty());
            });
            assert_matches!(&i1[..], [
                Instruction::Const(_, ConstantValue::I32(123)),
                Instruction::Branch(BranchTarget {
                    block_index: BlockId(2),
                    arguments: a
                })
            ] => {
                assert!(a.is_empty());
            });
            assert!(i2.is_empty());
        });
    }

    #[test]
    fn else_no_value() {
        // arrange
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![Statement::Expression(Expression::int_literal(0, 1))],
                None,
            ),
            Some(Expression::block(
                0,
                vec![Statement::Expression(Expression::int_literal(0, 2))],
                None,
            )),
        );
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_if_expr(&mut ir, &expr);

        // assert
        assert_eq!(var_id, None);
        assert_eq!(function.locals.len(), 3);

        assert_matches!(&function.blocks[..], [
            Block {
                params: p0,
                instructions: i0,
            },
            Block {
                params: p1,
                instructions: i1,
            },
            Block {
                params: p2,
                instructions: i2,
            },
            Block {
                params: p3,
                instructions: i3
            },
        ] => {
            assert!(p0.is_empty());
            assert!(p1.is_empty());
            assert!(p2.is_empty());
            assert!(p3.is_empty());

            assert_matches!(&i0[..], [
                Instruction::Const(v1a, ConstantValue::Bool(true)),
                Instruction::BranchIf(v1b, BranchTarget {
                    block_index: BlockId(1),
                    arguments: a1,
                }, BranchTarget {
                    block_index: BlockId(2),
                    arguments: a2
                }),
            ] => {
                assert_eq!(v1a, v1b);
                assert!(a1.is_empty());
                assert!(a2.is_empty());
            });
            assert_matches!(&i1[..], [
                Instruction::Const(_, ConstantValue::I32(1)),
                Instruction::Branch(BranchTarget {
                    block_index: BlockId(3),
                    arguments: a
                })
            ] => {
                assert!(a.is_empty());
            });
            assert_matches!(&i2[..], [
                Instruction::Const(_, ConstantValue::I32(2)),
                Instruction::Branch(BranchTarget {
                    block_index: BlockId(3),
                    arguments: a
                })
            ] => {
                assert!(a.is_empty());
            });

            assert!(i3.is_empty());
        });
    }

    #[test]
    fn else_value() {
        // arrange
        let mut expr = Expression::if_(
            0,
            Expression::bool_literal(0, true),
            Expression::block(
                0,
                vec![],
                Some(Expression::int_literal(0, 1)),
            ),
            Some(Expression::block(
                0,
                vec![],
                Some(Expression::int_literal(0, 2)),
            )),
        );
        let mut function = Function::new();
        let mut ir = IrBuilder::new(&mut function);
        let scope = Scope {};

        calculate_expression_type(&mut expr, &scope);

        // act
        let var_id = generate_ir_for_if_expr(&mut ir, &expr);

        // assert
        assert_eq!(function.locals.len(), 4);

        assert_matches!(&function.blocks[..], [
            Block {
                params: p0,
                instructions: i0,
            },
            Block {
                params: p1,
                instructions: i1,
            },
            Block {
                params: p2,
                instructions: i2,
            },
            Block {
                params: p3,
                instructions: i3
            },
        ] => {
            assert!(p0.is_empty());
            assert!(p1.is_empty());
            assert!(p2.is_empty());
            assert_eq!(p3, &vec![var_id.unwrap()]);

            assert_matches!(&i0[..], [
                Instruction::Const(v1a, ConstantValue::Bool(true)),
                Instruction::BranchIf(v1b, BranchTarget {
                    block_index: BlockId(1),
                    arguments: a1,
                }, BranchTarget {
                    block_index: BlockId(2),
                    arguments: a2
                }),
            ] => {
                assert_eq!(v1a, v1b);
                assert!(a1.is_empty());
                assert!(a2.is_empty());
            });
            assert_matches!(&i1[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Branch(BranchTarget {
                    block_index: BlockId(3),
                    arguments: a
                })
            ] => {
                assert_eq!(a, &vec![*v1]);
            });
            assert_matches!(&i2[..], [
                Instruction::Const(v1, ConstantValue::I32(2)),
                Instruction::Branch(BranchTarget {
                    block_index: BlockId(3),
                    arguments: a
                })
            ] => {
                assert_eq!(a, &vec![*v1]);
            });

            assert!(i3.is_empty());
        });
    }
}
