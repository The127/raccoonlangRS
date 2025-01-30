use crate::ast::expressions::Expression;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression {
    pub(super) span_: Span,
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Option<Box<Expression>>,
}

impl HasSpan for IfExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}