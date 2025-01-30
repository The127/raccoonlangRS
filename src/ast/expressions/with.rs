use crate::ast::expressions::arg::Arg;
use crate::ast::expressions::Expression;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct WithExpression {
    pub(super) span_: Span,
    pub target: Box<Expression>,
    pub values: Vec<Arg>,
}

impl HasSpan for WithExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}