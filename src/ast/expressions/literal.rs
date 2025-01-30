use std::cmp::Ordering;
use crate::source_map::{HasSpan, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LiteralExpression {
    pub(super) span_: Span,
    pub value: LiteralValue,
}

impl HasSpan for LiteralExpression {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Copy, Clone)]
pub enum LiteralValue {
    I32(i32),
    U32(u32),
    F32(f32),
    Boolean(bool),
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LiteralValue::I32(v1) => matches!(other, LiteralValue::I32(v2) if v1 == v2),
            LiteralValue::U32(v1) => matches!(other, LiteralValue::U32(v2) if v1 == v2),
            LiteralValue::Boolean(v1) => matches!(other, LiteralValue::Boolean(v2) if v1 == v2),
            LiteralValue::F32(v1) => {
                matches!(other, LiteralValue::F32(v2) if v1.total_cmp(v2) == Ordering::Equal)
            }
        }
    }
}

impl Eq for LiteralValue {}
