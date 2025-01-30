use ustr::Ustr;
use crate::ast::expressions::Expression;
use crate::parser::Spanned;
use crate::source_map::{HasSpan, Span};

// args are not expressions, but they are used in different expressions so they live here

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Arg {
    Named(NamedArg),
    Unnamed(Expression),
}

impl Arg {
    pub fn named<S: Into<Span>>(span: S, name: Spanned<Ustr>, value: Expression) -> Self {
        Arg::Named(NamedArg {
            span_: span.into(),
            name: name,
            value: value,
            shorthand: false,
        })
    }

    pub fn shorthand<S: Into<Span>>(span: S, name: Spanned<Ustr>, value: Expression) -> Self {
        Arg::Named(NamedArg {
            span_: span.into(),
            name: name,
            value: value,
            shorthand: true,
        })
    }

    pub fn unnamed(value: Expression) -> Self {
        Arg::Unnamed(value)
    }
}

impl HasSpan for Arg {
    #[mutants::skip]
    fn span(&self) -> Span {
        match self {
            Arg::Named(x) => x.span(),
            Arg::Unnamed(x) => x.span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedArg {
    pub(super) span_: Span,
    pub name: Spanned<Ustr>,
    pub value: Expression,
    pub shorthand: bool,
}

impl HasSpan for NamedArg {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}