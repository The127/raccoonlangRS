use crate::ast::expressions::Expression;
use crate::parser::Spanned;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryExpression {
    pub(super) span_: Span,
    pub op: Spanned<BinaryOperator>,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Equals,
    NotEquals,
    LessThanOrEquals,
    GreaterThanOrEquals,
    LessThan,
    GreaterThan,
}

impl HasSpan for BinaryExpression {
    fn span(&self) -> Span {
        self.span_
    }
}