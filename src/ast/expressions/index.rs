use crate::ast::expressions::Expression;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexExpression {
    pub(super) span_: Span,
    pub target: Box<Expression>,
    pub args: Vec<Expression>,
}

impl HasSpan for IndexExpression {
    fn span(&self) -> Span {
        self.span_
    }
}