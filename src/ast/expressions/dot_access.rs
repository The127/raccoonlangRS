use ustr::Ustr;
use crate::ast::expressions::Expression;
use crate::parser::Spanned;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DotAccessExpression {
    pub(super) span_: Span,
    pub target: Box<Expression>,
    pub name: Spanned<Ustr>,
}

impl HasSpan for DotAccessExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}