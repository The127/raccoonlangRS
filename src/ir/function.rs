use crate::ast::function_decl::FunctionDecl;
use crate::types::type_ref::{BuiltinType, TypeRef};
use crate::errors::Errors;
use crate::ir::access::{generate_ir_for_access_expr, get_access_var};
use crate::ir::binary::generate_ir_for_binary_expr;
use crate::ir::block::generate_ir_for_block_expr;
use crate::ir::function_ir_builder::{BlockId, FunctionIrBuilder};
use crate::ir::ids::{SignatureId, TypeId, VarId};
use crate::ir::if_::generate_ir_for_if_expr;
use crate::ir::literal::generate_ir_for_literal_expr;
use crate::ir::tuple::generate_ir_for_tuple_expr;
use crate::ir::ConstantValue;
use crate::scope::ir::IrVarScope;
use std::fmt::{Display, Formatter};
use ustr::Ustr;
use crate::ast::expressions::{Expression, ExpressionKind, TypeCoercionHint};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct FunctionSignature {
    pub parameters: Vec<TypeId>,
    pub return_: TypeId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    pub name: Option<Ustr>,
    pub signature: SignatureId,
    pub param_names: Option<Vec<Ustr>>,
    pub locals: Vec<(VarId, TypeId)>, // TODO: locals should have (optional) names like params // TODO: should locals include function params? maybe params get their own namespace?
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: None,
            signature: SignatureId::empty(),
            param_names: None,
            locals: vec![],
            blocks: vec![Block {
                instructions: vec![],
            }],
        }
    }
}

impl Display for Function {
    #[mutants::skip]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.signature == SignatureId::empty() {
            writeln!(f, "fn {}:", self.name.unwrap())?;
        } else {
            writeln!(f, "fn {}: {}:", self.name.unwrap(), self.signature)?;
        }

        for (idx, block) in self.blocks.iter().enumerate() {
            writeln!(f, "b_{}:", idx)?;
            for instr in &block.instructions {
                writeln!(f, "    {}", instr)?;
            }
        }
        writeln!(f, "")?;
        Ok(())
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
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Const(VarId, ConstantValue),
    Assign(VarId, VarId),
    Add(VarId, VarId, VarId),
    Sub(VarId, VarId, VarId),
    Mul(VarId, VarId, VarId),
    Div(VarId, VarId, VarId),
    Equals(VarId, VarId, VarId),
    NotEquals(VarId, VarId, VarId),
    GreaterThan(VarId, VarId, VarId),
    GreaterThanOrEquals(VarId, VarId, VarId),
    LessThan(VarId, VarId, VarId),
    LessThanOrEquals(VarId, VarId, VarId),
    Tuple(VarId, Vec<VarId>),
    TupleAccess(VarId, VarId, usize),
    Branch(BlockId),
    BranchIf(VarId, BlockId, BlockId),
    Return(VarId),
}

impl Display for Instruction {
    #[mutants::skip]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let (dest, str) = match self {
            Instruction::Const(d, v1) => (*d, format!("{}", v1)),
            Instruction::Assign(d, v1) => (*d, format!("{}", v1)),
            Instruction::Add(d, v1, v2) => (*d, format!("{} + {}", v1, v2)),
            Instruction::Sub(d, v1, v2) => (*d, format!("{} - {}", v1, v2)),
            Instruction::Mul(d, v1, v2) => (*d, format!("{} * {}", v1, v2)),
            Instruction::Div(d, v1, v2) => (*d, format!("{} / {}", v1, v2)),
            Instruction::Equals(d, v1, v2) => (*d, format!("{} == {}", v1, v2)),
            Instruction::NotEquals(d, v1, v2) => (*d, format!("{} != {}", v1, v2)),
            Instruction::GreaterThan(d, v1, v2) => (*d, format!("{} > {}", v1, v2)),
            Instruction::GreaterThanOrEquals(d, v1, v2) => (*d, format!("{} >= {}", v1, v2)),
            Instruction::LessThan(d, v1, v2) => (*d, format!("{} < {}", v1, v2)),
            Instruction::LessThanOrEquals(d, v1, v2) => (*d, format!("{} <= {}", v1, v2)),
            Instruction::Tuple(d, vars) => (
                *d,
                format!(
                    "({})",
                    vars.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            ),
            Instruction::TupleAccess(d, var, idx) => (*d, format!("{} . {}", var, idx)),
            Instruction::Branch(target) => (VarId::discard(), format!("br b_{}", target.0)),
            Instruction::BranchIf(cond, target_1, target_2) => (
                VarId::discard(),
                format!("br if {} then b_{} else b_{}", cond, target_1.0, target_2.0),
            ),
            Instruction::Return(v) => (VarId::discard(), format!("return {}", v)),
        };

        if dest == VarId::discard() {
            write!(f, "{}", str)?;
        } else {
            write!(f, "{} = {}", dest, str)?;
        }
        Ok(())
    }
}

pub fn generate_function_ir(ir: &mut FunctionIrBuilder, decl: &FunctionDecl, errors: &mut Errors) {
    ir.set_name(decl.name);

    // TODO: visibility and signature

    let sig_id = ir.map_signature(
        decl.return_type.type_ref.as_ref().unwrap(),
        decl.parameters
            .iter()
            .map(|x| x.type_ref.as_ref().unwrap())
            .collect(),
    );
    ir.set_signature(sig_id);

    ir.set_param_names(Some(decl.parameters.iter().map(|x| x.name).collect()));

    let target_var = match &decl.return_type.type_ref {
        Some(TypeRef::Builtin(BuiltinType::Unit)) => None,
        Some(ret_type) => {
            let type_id = ir.map_type(ret_type);
            Some(ir.create_local(type_id))
        }
        _ => None,
    };

    let mut scope = IrVarScope::new();
    let mut param_id = 0;
    for param in &decl.parameters {
        scope.insert(param.name, VarId::param(param_id));
        param_id += 1;
    }

    generate_ir_for_expr(ir, &scope, target_var, &decl.body, errors);
    if let Some(return_var) = target_var {
        ir.instr(Instruction::Return(return_var));
    }
}

pub(super) fn generate_ir_for_expr(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    target: Option<VarId>,
    expression: &Expression,
    errors: &mut Errors,
) {
    match expression.kind {
        ExpressionKind::Literal(_) => {
            generate_ir_for_literal_expr(ir, target.unwrap_or(VarId::discard()), expression)
        }
        ExpressionKind::Binary(_) => generate_ir_for_binary_expr(
            ir,
            scope,
            target.unwrap_or(VarId::discard()),
            expression,
            errors,
        ),
        ExpressionKind::Access(_) => {
            generate_ir_for_access_expr(ir, scope, target.unwrap_or(VarId::discard()), expression)
        }
        ExpressionKind::If(_) => generate_ir_for_if_expr(ir, scope, target, expression, errors),
        ExpressionKind::Block(_) => {
            generate_ir_for_block_expr(ir, scope, target, expression, errors)
        }
        ExpressionKind::Tuple(_) => {
            generate_ir_for_tuple_expr(ir, scope, target.unwrap_or(VarId::discard()), expression, errors)
        }
        _ => {
            dbg!(expression);
            todo!()
        }
    }
}

pub(super) fn generate_ir_for_expr_as_var(
    ir: &mut FunctionIrBuilder,
    scope: &IrVarScope,
    expression: &Expression,
    errors: &mut Errors,
) -> VarId {
    match &expression.kind {
        ExpressionKind::Access(access) => get_access_var(ir, scope, access),
        _ => {
            let type_id = ir.map_type(&expression.get_type(TypeCoercionHint::NoCoercion, errors));
            let var = ir.create_local(type_id);
            generate_ir_for_expr(ir, scope, Some(var), expression, errors);
            var
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::function_decl::{
        FunctionDecl, FunctionParameter as AstFunctionParameter, FunctionReturnType,
    };
    use crate::ast::path::Path;
    use crate::ast::statement::Statement;
    use crate::ast::types::{NamedType, Type};
    use crate::types::type_ref::{BuiltinType, TypeRef};
    use crate::ast::Visibility as AstVisibility;
    use crate::ir::test::IrTestEnv;
    use crate::parser::ToSpanned;
    use assert_matches::assert_matches;
    use ustr::ustr;
    use crate::ast::expressions::binary::BinaryOperator;

    #[test]
    fn empty_function() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut func_decl = FunctionDecl::new(
            0,
            None,
            AstVisibility::Module,
            vec![],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: Some(TypeRef::Builtin(BuiltinType::Unit)),
            },
            Expression::block(0, vec![], None),
        );
        env.typecheck_expression(&mut func_decl.body);

        // act
        let func_id = generate_function_ir(&mut env.function_ir_builder, &func_decl, &mut errors);

        // assert
        assert_eq!(
            env.get_function(),
            &Function {
                name: None,
                signature: SignatureId::empty(),
                param_names: Some(vec![]),
                locals: vec![],
                blocks: vec![Block {
                    instructions: vec![],
                }],
            }
        )
    }

    #[test]
    fn return_value() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut func_decl = FunctionDecl::new(
            0,
            None,
            AstVisibility::Module,
            vec![],
            FunctionReturnType {
                type_: Type::Named(NamedType::new(0, Path::name("i32"))),
                type_ref: Some(TypeRef::Builtin(BuiltinType::I32)),
            },
            Expression::block(0, vec![], Some(Expression::i32_literal(0, 42))),
        );
        env.typecheck_expression(&mut func_decl.body);

        // act
        generate_function_ir(&mut env.function_ir_builder, &func_decl, &mut errors);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 1);
        let (v1, _) = func.locals[0];
        assert_eq!(
            func.blocks,
            vec![Block {
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
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut func_decl = FunctionDecl::new(
            0,
            None,
            AstVisibility::Module,
            vec![],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: Some(TypeRef::Builtin(BuiltinType::Unit)),
            },
            Expression::block(
                0,
                vec![
                    Statement::Expression(Expression::i32_literal(0, 1)),
                    Statement::Expression(Expression::i32_literal(0, 2)),
                    Statement::Expression(Expression::i32_literal(0, 3)),
                ],
                None,
            ),
        );
        env.typecheck_expression(&mut func_decl.body);

        // act
        let func_id = generate_function_ir(&mut env.function_ir_builder, &func_decl, &mut errors);

        // assert
        let func = env.get_function();
        assert!(func.locals.is_empty());

        assert_matches!(&func.blocks[..], [
            Block {
                instructions: i,
            }
        ] => {
            assert_matches!(i[..], [
                Instruction::Const(d1, ConstantValue::I32(1)),
                Instruction::Const(d2, ConstantValue::I32(2)),
                Instruction::Const(d3, ConstantValue::I32(3)),
            ] => {
                assert_eq!(d1, VarId::discard());
                assert_eq!(d2, VarId::discard());
                assert_eq!(d3, VarId::discard());
            });
        });
    }

    // #[test]
    fn branch() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let body = Expression::block(
            0,
            vec![],
            Some(Expression::if_(
                0,
                Expression::i32_literal(0, 0),
                Expression::block(
                    0,
                    vec![],
                    Some(Expression::binary(
                        0,
                        BinaryOperator::Add.spanned_empty(),
                        Expression::i32_literal(0, 1),
                        Expression::i32_literal(0, 2),
                    )),
                ),
                Some(Expression::block(
                    0,
                    vec![],
                    Some(Expression::i32_literal(0, 3)),
                )),
            )),
        );
        let mut func_decl = FunctionDecl::new(
            0,
            None,
            AstVisibility::Module,
            vec![],
            FunctionReturnType {
                type_: Type::Unit,
                type_ref: Some(TypeRef::Builtin(BuiltinType::Unit)),
            },
            body,
        );
        env.typecheck_expression(&mut func_decl.body);

        // act
        let func_id = generate_function_ir(&mut env.function_ir_builder, &func_decl, &mut errors);

        // assert
        let func = env.get_function();
        assert_eq!(func.locals.len(), 3);

        assert_matches!(&func.blocks[..], [
            Block {
                instructions: i1,
            },
            Block {
                instructions: i2,
            },
            Block {
                instructions: i3,
            },
            Block {
                instructions: i4,
            },
        ] => {

            assert_matches!(&i1[..], [
                Instruction::Const(v1a, ConstantValue::I32(0)),
                Instruction::BranchIf(
                    v1b,
                    BlockId(1),
                    BlockId(2),
                )
            ] => {
                assert_eq!(v1a, v1b);
            });

            assert_matches!(&i2[..], [
                 Instruction::Const(v2a, ConstantValue::I32(1)),
                    Instruction::Const(v3a, ConstantValue::I32(2)),
                    Instruction::Add(discard, v2b, v3b),
                    Instruction::Branch(BlockId(3))
            ] => {
                assert_eq!(v2a, v2b);
                assert_eq!(v3a, v3b);
                assert_eq!(discard, &VarId::discard());
            });

            assert_matches!(&i3[..], [
                Instruction::Const(discard, ConstantValue::I32(3)),
                    Instruction::Branch(BlockId(3))
            ] => {
                assert_eq!(discard, &VarId::discard());
            });

            assert!(i4.is_empty());
        });
    }

    #[test]
    fn params() {
        // arrange
        let mut errors = Errors::new();
        let mut env = IrTestEnv::new();
        let mut func_decl = FunctionDecl::new(
            0,
            None,
            AstVisibility::Module,
            vec![AstFunctionParameter::new(
                0,
                ustr("foo"),
                Type::Named(NamedType::new(0, Path::name("i32"))),
            )
            .with_type_ref(TypeRef::Builtin(BuiltinType::I32))],
            FunctionReturnType {
                type_: Type::Named(NamedType::new(0, Path::name("i32"))),
                type_ref: Some(TypeRef::Builtin(BuiltinType::I32)),
            },
            Expression::access(0, Path::name("foo")),
        );

        env.typecheck_function(&mut func_decl);

        // act
        let func_id = generate_function_ir(&mut env.function_ir_builder, &func_decl, &mut errors);

        // assert
        let func = env.get_function();
        assert_matches!(func.blocks[0].instructions[..], [
            Instruction::Assign(v1a, v2),
            Instruction::Return(v1b),
        ] => {
            assert_eq!(v1a, v1b);
            assert_eq!(v2, VarId::param(0));
        })
    }

    // TODO: test that params are put into the function, and test that they are in the scope (by doing an access on a param in the body)
}
