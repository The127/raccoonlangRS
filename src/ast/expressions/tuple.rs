use crate::ast::expressions::Expression;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TupleExpression {
    pub(super) span_: Span,
    pub values: Vec<Expression>,
}

impl HasSpan for TupleExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}