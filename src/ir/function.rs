use crate::ast::expressions::Expression;
use crate::ir::add::generate_ir_for_add_expr;
use crate::ir::block::generate_ir_for_block_expr;
use crate::ir::ids::{TypeId, VarId};
use crate::ir::if_::generate_ir_for_if_expr;
use crate::ir::ir_builder::{BlockId, IrBuilder};
use crate::ir::literal::generate_ir_for_literal_expr;
use crate::ir::{ConstantValue, Visibility};
use assert_matches::assert_matches;
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    pub name: Option<Ustr>,
    pub visibility: Visibility,
    pub parameters: Vec<FunctionParameter>,
    pub return_type_id: TypeId,
    pub locals: Vec<(VarId, TypeId)>,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: None,
            visibility: Visibility::Module,
            parameters: vec![],
            return_type_id: TypeId::unit(),
            locals: vec![],
            blocks: vec![],
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionParameter {
    pub name: Option<Ustr>,
    pub type_id: TypeId,
    pub var_id: VarId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block {
    pub params: Vec<VarId>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Add(VarId, VarId, VarId),
    Sub(VarId, VarId, VarId),
    Const(VarId, ConstantValue),
    CmpEquals(VarId, VarId, VarId),
    CmpNotEquals(VarId, VarId, VarId),
    CmpGreaterThan(VarId, VarId, VarId),
    CmpGreaterThanOrEquals(VarId, VarId, VarId),
    Branch(BranchTarget),
    BranchIf(VarId, BranchTarget, BranchTarget),
    Return(VarId),
}

#[derive(Debug, Eq, PartialEq)]
pub struct BranchTarget {
    pub block_index: BlockId,
    pub arguments: Vec<VarId>,
}

impl BranchTarget {
    pub fn new(block: BlockId) -> Self {
        Self {
            block_index: block,
            arguments: vec![],
        }
    }

    pub fn with_args(block: BlockId, arguments: Vec<VarId>) -> Self {
        Self {
            block_index: block,
            arguments,
        }
    }
}

fn generate_function_ir(params: Vec<FunctionParameter>, body: &Expression) -> Function {
    let body = assert_matches!(body, Expression::Block(x) => x);

    let mut function = Function::new();

    let mut ir = IrBuilder::new(&mut function);

    let result = generate_ir_for_block_expr(&mut ir, body);
    if let Some(return_var) = result {
        ir.instr(Instruction::Return(return_var));
    }

    function
}

pub(super) fn generate_ir_for_expr(ir: &mut IrBuilder, expression: &Expression) -> Option<VarId> {
    match expression {
        Expression::Literal(x) => Some(generate_ir_for_literal_expr(ir, x)),
        Expression::Add(x) => Some(generate_ir_for_add_expr(ir, x)),
        Expression::If(x) => generate_ir_for_if_expr(ir, x),
        Expression::Block(x) => generate_ir_for_block_expr(ir, x),
        _ => {
            dbg!(expression);
            todo!()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::{AddExpressionOperator, Expression};
    use crate::ast::statement::Statement;

    #[test]
    fn empty_function() {
        // arrange
        let params = vec![];
        let body = Expression::block(0, vec![], None);

        // act
        let func = generate_function_ir(params, &body);

        // assert
        assert_eq!(
            func,
            Function {
                name: None,
                visibility: Visibility::Module,
                parameters: vec![],
                return_type_id: TypeId::unit(),
                locals: vec![],
                blocks: vec![Block {
                    params: vec![],
                    instructions: vec![],
                }],
            }
        )
    }

    #[test]
    fn return_value() {
        // arrange
        let params = vec![];
        let body = Expression::block(0, vec![], Some(Expression::int_literal(0, 42)));

        // act
        let func = generate_function_ir(params, &body);

        // assert
        assert_eq!(func.locals.len(), 1);
        let (v1, _) = func.locals[0];
        assert_eq!(
            func.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(42)),
                    Instruction::Return(v1),
                ]
            }]
        );
    }

    #[test]
    fn statements() {
        // arrange
        let params = vec![];
        let body = Expression::block(
            0,
            vec![
                Statement::Expression(Expression::int_literal(0, 1)),
                Statement::Expression(Expression::int_literal(0, 2)),
                Statement::Expression(Expression::int_literal(0, 3)),
            ],
            None,
        );

        // act
        let func = generate_function_ir(params, &body);

        // assert
        assert_eq!(func.locals.len(), 3);

        assert_matches!(&func.blocks[..], [
            Block {
                params: p,
                instructions: i,
            }
        ] => {
            assert!(p.is_empty());
            assert_matches!(i[..], [
                Instruction::Const(v1, ConstantValue::I32(1)),
                Instruction::Const(v2, ConstantValue::I32(2)),
                Instruction::Const(v3, ConstantValue::I32(3)),
            ] => {
                assert!(func.locals.contains(&(v1, TypeId::i32())));
                assert!(func.locals.contains(&(v2, TypeId::i32())));
                assert!(func.locals.contains(&(v3, TypeId::i32())));
                assert_ne!(v1, v2);
                assert_ne!(v2, v3);
                assert_ne!(v1, v3);
            });
        });
    }

    #[test]
    fn branch() {
        // arrange
        let params = vec![];
        let body = Expression::block(
            0,
            vec![],
            Some(Expression::if_(
                0,
                Expression::int_literal(0, 0),
                Expression::block(
                    0,
                    vec![],
                    Some(Expression::add(
                        0,
                        Expression::int_literal(0, 1),
                        vec![(AddExpressionOperator::OpPlus, Expression::int_literal(0, 2))],
                    )),
                ),
                Some(Expression::block(
                    0,
                    vec![],
                    Some(Expression::int_literal(0, 3)),
                )),
            )),
        );

        // act
        let func = generate_function_ir(params, &body);

        // assert
        assert_eq!(func.locals.len(), 6);

        assert_matches!(&func.blocks[..], [
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
                instructions: i3,
            },
            Block {
                params: p4,
                instructions: i4,
            },
        ] => {
            assert!(p1.is_empty());
            assert!(p2.is_empty());
            assert!(p3.is_empty());
            assert_eq!(p4.len(), 1);

            assert_matches!(&i1[..], [
                Instruction::Const(v1a, ConstantValue::I32(0)),
                Instruction::BranchIf(
                    v1b,
                    BranchTarget {block_index: BlockId(1), arguments: args1},
                    BranchTarget {block_index: BlockId(2), arguments: args2},
                )
            ] => {
                assert_eq!(v1a, v1b);
                assert!(args1.is_empty());
                assert!(args2.is_empty());
            });

            assert_matches!(&i2[..], [
                 Instruction::Const(v2a, ConstantValue::I32(1)),
                    Instruction::Const(v3a, ConstantValue::I32(2)),
                    Instruction::Add(v4, v2b, v3b),
                    Instruction::Branch(BranchTarget {
                        block_index: BlockId(3),
                        arguments: args,
                    })
            ] => {
                assert_eq!(v2a, v2b);
                assert_eq!(v3a, v3b);
                assert_eq!(args, &vec![*v4]);
            });

            assert_matches!(&i3[..], [
                Instruction::Const(v5, ConstantValue::I32(3)),
                    Instruction::Branch(BranchTarget {
                        block_index: BlockId(3),
                        arguments: args,
                    })
            ] => {
                assert_eq!(args, &vec![*v5]);
            });

            assert_matches!(i4[..], [
                Instruction::Return(v6),
            ] => {
                assert_eq!(p4, &vec![v6]);
            });
        });
    }
}
