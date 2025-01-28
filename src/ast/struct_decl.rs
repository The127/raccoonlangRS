use std::cell::RefMut;
use crate::ast::types::Type;
use crate::ast::Visibility;
use crate::source_map::{HasSpan, Span};
use ustr::Ustr;
use crate::refs::MutableRef;
use crate::types::type_ref::{StructType, TypeRef};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDecl {
    span_: Span,
    pub name: Option<Ustr>,
    pub visibility: Visibility,
    pub members: Vec<StructMember>,
    struct_type: MutableRef<StructType>,
}

impl StructDecl {
    pub fn new<S: Into<Span>>(span: S, name: Option<Ustr>, visibility: Visibility, members: Vec<StructMember>) -> Self {
        Self {
            span_: span.into(),
            name,
            visibility,
            members,
            struct_type: Default::default(),
        }
    }

    pub fn get_typeref(&self) -> TypeRef {
        TypeRef::Struct(self.struct_type.get_immutable())
    }

    pub fn borrow_struct_type_mut(&self) -> RefMut<StructType> {
        self.struct_type.borrow_mut()
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
