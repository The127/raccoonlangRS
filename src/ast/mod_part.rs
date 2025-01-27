use crate::ast::function_decl::FunctionDecl;
use crate::ast::struct_decl::StructDecl;
use crate::source_map::{HasSpan, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ModPart {
    span_: Span,
    pub path: Vec<Ustr>,
    // pub uses: Uses, //TODO:
    pub functions: Vec<FunctionDecl>,
    pub structs: Vec<StructDecl>,
}

impl ModPart {
    pub fn new<S: Into<Span>>(span: S, path: Vec<Ustr>) -> Self {
        ModPart {
            span_: span.into(),
            path: path,
            functions: vec![],
            structs: vec![],
        }
    }

    pub fn set_span<S: Into<Span>>(&mut self, span: S) {
        self.span_ = span.into();
    }

    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

impl HasSpan for ModPart {
    fn span(&self) -> Span {
        self.span_
    }
}

pub struct Uses {}

