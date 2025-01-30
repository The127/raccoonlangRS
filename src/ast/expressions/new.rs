use crate::ast::expressions::arg::Arg;
use crate::ast::path::Path;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NewExpression {
    pub(super) span_: Span,
    pub path: Path,
    pub args: Vec<Arg>,
}


impl HasSpan for NewExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}