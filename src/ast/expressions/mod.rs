pub mod block;
pub mod access;
pub mod literal;
pub mod binary;
pub mod unknown;
pub mod if_;
pub mod tuple;

use crate::ast::expressions::access::{transform_access_expression, AccessExpression};
use crate::ast::expressions::binary::{transform_add_expression, transform_compare_expression, transform_mul_expression, BinaryExpression, BinaryOperator};
use crate::ast::expressions::block::{transform_block_expression, BlockExpression, LetDeclaration};
use crate::ast::expressions::if_::{transform_if_expression, IfExpression};
use crate::ast::expressions::literal::{transform_literal_expression, LiteralExpression, LiteralValue};
use crate::ast::expressions::tuple::{transform_tuple_expression, TupleExpression};
use crate::ast::expressions::unknown::UnknownExpression;
use crate::ast::path::Path;
use crate::ast::statement::Statement;
use crate::ast::typing::TypeRef;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_ref: Option<TypeRef>,
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

    pub fn block_with_decl<S: Into<Span>>(span: S, implicit: bool, decl: LetDeclaration, statements: Vec<Statement>, value: Option<Expression>) -> Self {
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

    pub fn tuple<S: Into<Span>>(
        span: S,
        values: Vec<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::Tuple(TupleExpression {
                span_: span.into(),
                values,
            }),
            type_ref: None,
        }
    }

    pub fn int_literal<S: Into<Span>>(span: S, value: i32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::Integer(value),
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
        op: BinaryOperator,
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

pub fn transform_expression(node: &ExpressionNode, sources: &SourceCollection) -> Expression {
    match node {
        ExpressionNode::Literal(x) => transform_literal_expression(x, sources),
        ExpressionNode::Block(x) => transform_block_expression(x, sources),
        ExpressionNode::If(x) => transform_if_expression(x, sources),
        ExpressionNode::Add(x) => transform_add_expression(x, sources),
        ExpressionNode::Mul(x) => transform_mul_expression(x, sources),
        ExpressionNode::Compare(x) => transform_compare_expression(x, sources),
        ExpressionNode::Access(x) => transform_access_expression(x, sources),
        ExpressionNode::Tuple(x) => transform_tuple_expression(x, sources),
    }
}

#[cfg(test)]
mod test {

}
