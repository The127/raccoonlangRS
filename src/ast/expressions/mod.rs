pub mod access;
pub mod binary;
pub mod block;
pub mod if_;
pub mod literal;
pub mod tuple;
pub mod unknown;

use crate::add_error;
use crate::ast::expressions::access::{transform_access_expression, AccessExpression};
use crate::ast::expressions::binary::{
    transform_add_expression, transform_compare_expression, transform_mul_expression,
    BinaryExpression, BinaryOperator,
};
use crate::ast::expressions::block::{transform_block_expression, BlockExpression, LetDeclaration};
use crate::ast::expressions::if_::{transform_if_expression, IfExpression};
use crate::ast::expressions::literal::{
    transform_literal_expression, LiteralExpression, LiteralValue,
};
use crate::ast::expressions::tuple::{transform_tuple_expression, TupleExpression};
use crate::ast::expressions::unknown::UnknownExpression;
use crate::ast::path::Path;
use crate::ast::statement::Statement;
use crate::ast::typing::TypeRef;
use crate::errors::{ErrorKind, Errors};
use crate::parser::expression_node::ExpressionNode;
use crate::parser::Spanned;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    type_ref: Option<TypeRef>,
}

impl Expression {
    pub(crate) fn set_expression(&mut self, type_ref: TypeRef) {
        self.type_ref = Some(type_ref)
    }
}

#[cfg(test)]
impl Expression {
    pub fn type_ref(&self) -> Option<TypeRef> {
        self.type_ref.clone()
    }
}

impl HasSpan for Expression {
    fn span(&self) -> Span {
        self.kind.span()
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionKind {
    Literal(LiteralExpression),
    Access(AccessExpression),
    Block(BlockExpression),
    Binary(BinaryExpression),
    If(IfExpression),
    Tuple(TupleExpression),
    Unknown(UnknownExpression),
}

impl HasSpan for ExpressionKind {
    fn span(&self) -> Span {
        match self {
            ExpressionKind::Literal(x) => x.span(),
            ExpressionKind::Access(x) => x.span(),
            ExpressionKind::Block(x) => x.span(),
            ExpressionKind::Binary(x) => x.span(),
            ExpressionKind::If(x) => x.span(),
            ExpressionKind::Tuple(x) => x.span(),
            ExpressionKind::Unknown(x) => x.span(),
        }
    }
}

impl Expression {
    pub fn unknown() -> Self {
        Self {
            kind: ExpressionKind::Unknown(UnknownExpression {
                span_: Span::empty(),
            }),
            type_ref: None,
        }
    }

    pub fn block_with_decl<S: Into<Span>>(
        span: S,
        implicit: bool,
        decl: LetDeclaration,
        statements: Vec<Statement>,
        value: Option<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::Block(BlockExpression {
                span_: span.into(),
                implicit,
                let_: Some(decl),
                statements,
                value: value.map(Box::new),
            }),
            type_ref: None,
        }
    }

    pub fn block<S: Into<Span>>(
        span: S,
        statements: Vec<Statement>,
        value: Option<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::Block(BlockExpression {
                span_: span.into(),
                implicit: false,
                let_: None,
                statements,
                value: value.map(Box::new),
            }),
            type_ref: None,
        }
    }

    pub fn tuple<S: Into<Span>>(span: S, values: Vec<Expression>) -> Self {
        Self {
            kind: ExpressionKind::Tuple(TupleExpression {
                span_: span.into(),
                values,
            }),
            type_ref: None,
        }
    }

    pub fn i32_literal<S: Into<Span>>(span: S, value: i32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::I32(value),
            }),
            type_ref: None,
        }
    }

    pub fn u32_literal<S: Into<Span>>(span: S, value: u32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::U32(value),
            }),
            type_ref: None,
        }
    }

    pub fn f32_literal<S: Into<Span>>(span: S, value: f32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::F32(value),
            }),
            type_ref: None,
        }
    }

    pub fn bool_literal<S: Into<Span>>(span: S, value: bool) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::Boolean(value),
            }),
            type_ref: None,
        }
    }

    pub fn access<S: Into<Span>>(span: S, path: Path) -> Self {
        Self {
            kind: ExpressionKind::Access(AccessExpression {
                span_: span.into(),
                path,
            }),
            type_ref: None,
        }
    }

    pub fn binary<S: Into<Span>>(
        span: S,
        op: Spanned<BinaryOperator>,
        left: Expression,
        right: Expression,
    ) -> Self {
        Self {
            kind: ExpressionKind::Binary(BinaryExpression {
                span_: span.into(),
                op,
                left: Box::new(left),
                right: Box::new(right),
            }),
            type_ref: None,
        }
    }

    pub fn if_<S: Into<Span>>(
        span: S,
        condition: Expression,
        then: Expression,
        else_: Option<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::If(IfExpression {
                span_: span.into(),
                condition: Box::new(condition),
                then: Box::new(then),
                else_: else_.map(Box::new),
            }),
            type_ref: None,
        }
    }
}

pub enum TypeCoercionHint {
    NoCoercion,
    Any,
    Specific(TypeRef),
}

impl Expression {
    pub fn get_type(&self, hint: TypeCoercionHint, errors: &mut Errors) -> TypeRef {
        let type_ref = self.type_ref.as_ref().unwrap().clone();
        match hint {
            TypeCoercionHint::NoCoercion => type_ref,
            TypeCoercionHint::Any => {
                match type_ref {
                    TypeRef::Indeterminate(possibilities) => {
                        add_error!(errors, self.span(), IndeterminateType(possibilities.clone()));
                        TypeRef::Unknown
                    },
                    x => x,
                }
            },
            TypeCoercionHint::Specific(desired) => {
                // TODO: fully implement
                if type_ref != desired {
                    add_error!(errors, self.span(), TypeMismatch(type_ref.clone(), desired));
                    TypeRef::Unknown
                }else{
                    type_ref
                }
            },
        }
    }
}

pub fn transform_expression(
    node: &ExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    match node {
        ExpressionNode::Literal(x) => transform_literal_expression(x, errors, sources),
        ExpressionNode::Block(x) => transform_block_expression(x, errors, sources),
        ExpressionNode::If(x) => transform_if_expression(x, errors, sources),
        ExpressionNode::Add(x) => transform_add_expression(x, errors, sources),
        ExpressionNode::Mul(x) => transform_mul_expression(x, errors, sources),
        ExpressionNode::Compare(x) => transform_compare_expression(x, errors, sources),
        ExpressionNode::Access(x) => transform_access_expression(x, errors, sources),
        ExpressionNode::Tuple(x) => transform_tuple_expression(x, errors, sources),
        ExpressionNode::Subsequent(x) => todo!() /*transform_subsequent_expression(x, errors, sources)*/,
    }
}

#[cfg(test)]
mod test {}
