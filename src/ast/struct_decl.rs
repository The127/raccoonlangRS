use crate::ast::function_decl::FunctionParameter;
use crate::ast::types::Type::Unknown;
use crate::ast::{map_visibility, Visibility};
use crate::errors::Errors;
use crate::parser::fn_parameter_node::FnParameterNode;
use crate::parser::struct_node::{StructMemberNode, StructNode};
use crate::source_map::{HasSpan, SourceCollection, Span};
use ustr::Ustr;
use crate::ast::types::Type;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDecl {
    span_: Span,
    pub name: Option<Ustr>,
    pub visibility: Visibility,
    pub members: Vec<StructMember>,
}

impl StructDecl {
    pub fn new<S: Into<Span>>(span: S, name: Option<Ustr>, visibility: Visibility, members: Vec<StructMember>) -> Self {
        Self {
            span_: span.into(),
            name,
            visibility,
            members,
        }
    }
}

impl HasSpan for StructDecl {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructMember {
    span_: Span,
    pub name: Ustr,
    pub type_: Type,
}

impl HasSpan for StructMember {
    fn span(&self) -> Span {
        self.span_
    }
}

impl StructMember {
    pub fn new<S: Into<Span>>(span: S, name: Ustr, type_: Type) -> Self {
        Self {
            span_: span.into(),
            name,
            type_,
        }
    }
}
