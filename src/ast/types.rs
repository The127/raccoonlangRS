use crate::ast::path::Path;
use crate::source_map::HasSpan;
use crate::source_map::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Unknown,
    Unit,
    Named(NamedType),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedType {
    span_: Span,
    pub path: Path,
}

impl HasSpan for NamedType {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}

impl NamedType {
    pub fn new<S: Into<Span>>(span: S, path: Path) -> Self {
        Self {
            span_: span.into(),
            path,
        }
    }
}