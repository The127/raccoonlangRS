use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnknownExpression {
    pub(super) span_: Span,
}

impl HasSpan for UnknownExpression {
    fn span(&self) -> Span {
        self.span_
    }
}
