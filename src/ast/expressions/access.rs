use crate::ast::path::Path;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AccessExpression {
    pub(super) span_: Span,
    pub path: Path,
}

impl HasSpan for AccessExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}