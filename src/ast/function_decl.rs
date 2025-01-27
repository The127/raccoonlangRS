use crate::ast::expressions::Expression;
use crate::ast::typing::TypeRef;
use crate::ast::Visibility;
use crate::source_map::{HasSpan, Span};
use ustr::Ustr;
use crate::ast::types::Type;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionDecl {
    span_: Span,
    pub name: Option<Ustr>,
    pub visibility: Visibility,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: FunctionReturnType,
    pub body: Expression,
}

impl HasSpan for FunctionDecl {
    fn span(&self) -> Span {
        self.span_
    }
}

impl FunctionDecl {
    pub fn new<S: Into<Span>>(
        span: S,
        name: Option<Ustr>,
        visibility: Visibility,
        parameters: Vec<FunctionParameter>,
        return_type: FunctionReturnType,
        body: Expression,
    ) -> Self {
        Self {
            span_: span.into(),
            name,
            visibility,
            parameters,
            return_type,
            body,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionParameter {
    span_: Span,
    pub name: Ustr,
    pub type_: Type,
    pub type_ref: Option<TypeRef>,
}

impl FunctionParameter {
    pub fn new<S: Into<Span>>(span: S, name: Ustr, type_: Type) -> Self {
        Self {
            span_: span.into(),
            name,
            type_,
            type_ref: None,
        }
    }

    #[cfg(test)]
    pub fn with_type_ref(mut self, type_ref: TypeRef) -> Self {
        self.type_ref = Some(type_ref);
        self
    }
}

impl HasSpan for FunctionParameter {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionReturnType {
    pub type_: Type,
    pub type_ref: Option<TypeRef>,
}

impl FunctionReturnType {
    pub fn new(type_: Type) -> Self {
        Self {
            type_,
            type_ref: None,
        }
    }
}