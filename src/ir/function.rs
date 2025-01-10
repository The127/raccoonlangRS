use assert_matches::assert_matches;
use crate::ast::expressions::Expression;
use crate::ast::function_decl::FunctionParameter;
use crate::ir::add::generate_ir_for_add_expr;
use crate::ir::block::generate_ir_for_block_expr;
use crate::ir::ConstantValue;
use crate::ir::ids::{TypeId, VarId};
use crate::ir::if_::generate_ir_for_if_expr;
use crate::ir::literal::generate_ir_for_literal_expr;


#[derive(Debug, Eq, PartialEq)]
pub struct Ir {
    pub locals: Vec<(VarId, TypeId)>,
    pub blocks: Vec<Block>,
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
    pub(super) block_index: usize,
    pub(super) arguments: Vec<VarId>,
}

impl Ir {
    pub(super) fn new() -> Self {
        Self {
            locals: vec![],
            blocks: vec![Block {
                params: vec![],
                instructions: vec![],
            }],
        }
    }

    pub(super) fn push_block(&mut self, params: Vec<VarId>) {
        self.blocks.push(Block {
            params: params,
            instructions: vec![],
        })
    }

    pub(super) fn push_instr(&mut self, instr: Instruction) {
        self.blocks.last_mut().unwrap().instructions.push(instr);
    }

    pub(super) fn new_var(&mut self, type_id: TypeId) -> VarId {
        let var_id = VarId::new();
        self.locals.push((var_id, type_id));
        var_id
    }
}

fn generate_function_ir(params: Vec<FunctionParameter>, body: &Expression) -> Ir {
    let body = assert_matches!(body, Expression::Block(x) => x);
    let mut ir = Ir::new();

    let result = generate_ir_for_block_expr(&mut ir, body);
    if let Some(return_var) = result {
        ir.push_instr(Instruction::Return(return_var));
    }

    ir
}

pub(super) fn generate_ir_for_expr(ir: &mut Ir, expression: &Expression) -> Option<VarId> {
    match expression {
        Expression::Literal(x) => Some(generate_ir_for_literal_expr(ir, x)),
        Expression::Add(x) => Some(generate_ir_for_add_expr(ir, x)),
        Expression::If(x) => generate_ir_for_if_expr(ir, x),
        Expression::Block(x) => generate_ir_for_block_expr(ir, x),
        _ => {
            dbg!(expression);
            todo!()
        },
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
        let ir = generate_function_ir(params, &body);

        // assert
        assert_eq!(
            ir,
            Ir {
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
        let ir = generate_function_ir(params, &body);

        // assert
        assert_eq!(ir.locals.len(), 1);
        let (v1, _) = ir.locals[0];
        assert_eq!(
            ir.blocks,
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
        let body = Expression::block(0, vec![
            Statement::Expression(Expression::int_literal(0, 1)),
            Statement::Expression(Expression::int_literal(0, 2)),
            Statement::Expression(Expression::int_literal(0, 3)),
        ], None);

        // act
        let ir = generate_function_ir(params, &body);

        // assert
        assert_eq!(ir.locals.len(), 3);
        let (v1, _) = ir.locals[0];
        let (v2, _) = ir.locals[1];
        let (v3, _) = ir.locals[2];
        assert_eq!(
            ir.blocks,
            vec![Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(1)),
                    Instruction::Const(v2, ConstantValue::I32(2)),
                    Instruction::Const(v3, ConstantValue::I32(3)),
                ]
            }]
        )
    }

    #[test]
    fn branch() {
        // arrange
        let params = vec![];
        let body = Expression::block(0, vec![], Some(
            Expression::if_(
                0,
                Expression::int_literal(0, 0),
                Expression::block(0, vec![], Some(
                    Expression::add(
                        0,
                        Expression::int_literal(0, 1),
                        vec![(AddExpressionOperator::OpPlus, Expression::int_literal(0, 2))])
                )),
                Some(Expression::block(0, vec![], Some(Expression::int_literal(0, 3))))
            )));

        // act
        let ir = generate_function_ir(params, &body);

        // assert
        assert_eq!(ir.locals.len(), 6);
        let (v1, _) = ir.locals[0];
        let (v2, _) = ir.locals[1];
        let (v3, _) = ir.locals[2];
        let (v4, _) = ir.locals[3];
        let (v5, _) = ir.locals[4];
        let (v6, _) = ir.locals[5];

        assert_eq!(ir.blocks, vec![
            Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v1, ConstantValue::I32(0)),
                    Instruction::BranchIf(
                        v1,
                        BranchTarget { block_index: 1, arguments: vec![] },
                        BranchTarget { block_index: 2, arguments: vec![] }
                    )
                ],
            },
            Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v2, ConstantValue::I32(1)),
                    Instruction::Const(v3, ConstantValue::I32(2)),
                    Instruction::Add(v4, v2, v3),
                    Instruction::Branch(BranchTarget {
                        block_index: 3,
                        arguments: vec![v4],
                    })
                ]
            },
            Block {
                params: vec![],
                instructions: vec![
                    Instruction::Const(v5, ConstantValue::I32(3)),
                    Instruction::Branch(BranchTarget {
                        block_index: 3,
                        arguments: vec![v5],
                    })
                ]
            },
            Block {
                params: vec![v6],
                instructions: vec![
                    Instruction::Return(v6),
                ],
            }
        ])
    }
}
