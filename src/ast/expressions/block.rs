use crate::ast::expressions::Expression;
use crate::ast::pattern::Pattern;
use crate::ast::statement::Statement;
use crate::types::type_ref::TypeRef;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpression {
    pub(super) span_: Span,
    pub implicit: bool,
    pub let_: Option<LetDeclaration>,
    pub statements: Vec<Statement>,
    pub value: Option<Box<Expression>>,
}

impl BlockExpression {
    pub fn value_span(&self) -> Span {
        if let Some(value) = &self.value {
            value.span()
        } else {
            (self.span_.end() - 1).into()
        }
    }
}

impl HasSpan for BlockExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetDeclaration {
    span_: Span,
    pub binding: Pattern,
    pub value: Box<Expression>,
    pub type_ref: Option<TypeRef>,
}

impl LetDeclaration {
    pub fn new<S: Into<Span>>(span: S, binding: Pattern, value: Expression) -> Self {
        Self {
            span_: span.into(),
            binding,
            value: Box::new(value),
            type_ref: None,
        }
    }

    #[cfg(test)]
    pub fn with_type_ref(mut self, type_ref: TypeRef) -> Self {
        self.type_ref = Some(type_ref);
        self
    }
}

impl HasSpan for LetDeclaration {
    fn span(&self) -> Span {
        self.span_
    }
}
