use ustr::Ustr;
use crate::add_error;
use crate::ast::expressions::access::AccessExpression;
use crate::ast::expressions::arg::Arg;
use crate::ast::expressions::binary::{BinaryExpression, BinaryOperator};
use crate::ast::expressions::block::{BlockExpression, LetDeclaration};
use crate::ast::expressions::call::CallExpression;
use crate::ast::expressions::dot_access::DotAccessExpression;
use crate::ast::expressions::if_::IfExpression;
use crate::ast::expressions::index::IndexExpression;
use crate::ast::expressions::literal::{LiteralExpression, LiteralValue};
use crate::ast::expressions::tuple::TupleExpression;
use crate::ast::expressions::unknown::UnknownExpression;
use crate::ast::expressions::with::WithExpression;
use crate::ast::path::Path;
use crate::ast::statement::Statement;
use crate::types::type_ref::TypeRef;
use crate::errors::Errors;
use crate::parser::Spanned;
use crate::source_map::{HasSpan, Span};

pub mod access;
pub mod binary;
pub mod block;
pub mod if_;
pub mod literal;
pub mod call;
pub mod index;
pub mod with;
pub mod dot_access;
pub mod arg;
pub mod tuple;
pub mod unknown;


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    type_ref: Option<TypeRef>,
}

impl Expression {
    pub(crate) fn set_type_ref(&mut self, type_ref: TypeRef) {
        self.type_ref = Some(type_ref)
    }
}

#[cfg(test)]
impl Expression {
    pub fn type_ref(&self) -> Option<TypeRef> {
        self.type_ref.clone()
    }

    pub fn value_span(&self) -> Span {
        match &self.kind {
            ExpressionKind::Literal(x) => x.span(),
            ExpressionKind::Access(x) => x.span(),
            ExpressionKind::Block(x) => x.value_span(),
            ExpressionKind::Binary(x) => x.span(),
            ExpressionKind::If(x) => x.span(),
            ExpressionKind::Tuple(x) => x.span(),
            ExpressionKind::DotAccess(x) => x.span(),
            ExpressionKind::Call(x) => x.span(),
            ExpressionKind::Index(x) => x.span(),
            ExpressionKind::With(x) => x.span(),
            ExpressionKind::Unknown(x) => x.span(),
        }
    }
}

impl HasSpan for Expression {
    fn span(&self) -> Span {
        self.kind.span()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionKind {
    Literal(LiteralExpression),
    Access(AccessExpression),
    Block(BlockExpression),
    Binary(BinaryExpression),
    If(IfExpression),
    Tuple(TupleExpression),
    DotAccess(DotAccessExpression),
    Call(CallExpression),
    Index(IndexExpression),
    With(WithExpression),
    Unknown(UnknownExpression),
}

impl HasSpan for ExpressionKind {
    fn span(&self) -> Span {
        match self {
            ExpressionKind::Literal(x) => x.span(),
            ExpressionKind::Access(x) => x.span(),
            ExpressionKind::Block(x) => x.span(),
            ExpressionKind::Binary(x) => x.span(),
            ExpressionKind::If(x) => x.span(),
            ExpressionKind::Tuple(x) => x.span(),
            ExpressionKind::DotAccess(x) => x.span(),
            ExpressionKind::Call(x) => x.span(),
            ExpressionKind::Index(x) => x.span(),
            ExpressionKind::With(x) => x.span(),
            ExpressionKind::Unknown(x) => x.span(),
        }
    }
}

impl Expression {
    pub fn unknown() -> Self {
        Self {
            kind: ExpressionKind::Unknown(UnknownExpression {
                span_: Span::empty(),
            }),
            type_ref: None,
        }
    }

    pub fn block_with_decl<S: Into<Span>>(
        span: S,
        implicit: bool,
        decl: LetDeclaration,
        statements: Vec<Statement>,
        value: Option<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::Block(BlockExpression {
                span_: span.into(),
                implicit,
                let_: Some(decl),
                statements,
                value: value.map(Box::new),
            }),
            type_ref: None,
        }
    }

    pub fn block<S: Into<Span>>(
        span: S,
        statements: Vec<Statement>,
        value: Option<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::Block(BlockExpression {
                span_: span.into(),
                implicit: false,
                let_: None,
                statements,
                value: value.map(Box::new),
            }),
            type_ref: None,
        }
    }

    pub fn tuple<S: Into<Span>>(span: S, values: Vec<Expression>) -> Self {
        Self {
            kind: ExpressionKind::Tuple(TupleExpression {
                span_: span.into(),
                values,
            }),
            type_ref: None,
        }
    }

    pub fn i32_literal<S: Into<Span>>(span: S, value: i32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::I32(value),
            }),
            type_ref: None,
        }
    }

    pub fn u32_literal<S: Into<Span>>(span: S, value: u32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::U32(value),
            }),
            type_ref: None,
        }
    }

    pub fn f32_literal<S: Into<Span>>(span: S, value: f32) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::F32(value),
            }),
            type_ref: None,
        }
    }

    pub fn bool_literal<S: Into<Span>>(span: S, value: bool) -> Self {
        Self {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::Boolean(value),
            }),
            type_ref: None,
        }
    }

    pub fn access<S: Into<Span>>(span: S, path: Path) -> Self {
        Self {
            kind: ExpressionKind::Access(AccessExpression {
                span_: span.into(),
                path,
            }),
            type_ref: None,
        }
    }

    pub fn binary<S: Into<Span>>(
        span: S,
        op: Spanned<BinaryOperator>,
        left: Expression,
        right: Expression,
    ) -> Self {
        Self {
            kind: ExpressionKind::Binary(BinaryExpression {
                span_: span.into(),
                op,
                left: Box::new(left),
                right: Box::new(right),
            }),
            type_ref: None,
        }
    }

    pub fn if_<S: Into<Span>>(
        span: S,
        condition: Expression,
        then: Expression,
        else_: Option<Expression>,
    ) -> Self {
        Self {
            kind: ExpressionKind::If(IfExpression {
                span_: span.into(),
                condition: Box::new(condition),
                then: Box::new(then),
                else_: else_.map(Box::new),
            }),
            type_ref: None,
        }
    }

    pub fn call<S: Into<Span>>(span: S, target: Expression, args: Vec<Arg>) -> Self {
        Self {
            kind: ExpressionKind::Call(CallExpression {
                span_: span.into(),
                target: target.into(),
                args,
            }),
            type_ref: None,
        }
    }

    pub fn index<S: Into<Span>>(span: S, target: Expression, args: Vec<Expression>) -> Self {
        Self {
            kind: ExpressionKind::Index(IndexExpression {
                span_: span.into(),
                target: target.into(),
                args,
            }),
            type_ref: None,
        }
    }

    pub fn with<S: Into<Span>>(span: S, target: Expression, values: Vec<Arg>) -> Self {
        Self {
            kind: ExpressionKind::With(WithExpression {
                span_: span.into(),
                target: target.into(),
                values,
            }),
            type_ref: None,
        }
    }

    pub fn dot_access<S: Into<Span>>(span: S, target: Expression, name: Spanned<Ustr>) -> Self {
        Self {
            kind: ExpressionKind::DotAccess(DotAccessExpression {
                span_: span.into(),
                target: Box::new(target),
                name,
            }),
            type_ref: None,
        }
    }
}

pub enum TypeCoercionHint {
    NoCoercion,
    Any,
    Specific(TypeRef),
}

impl Expression {
    pub fn get_type(&self, hint: TypeCoercionHint, errors: &mut Errors) -> TypeRef {
        let type_ref = self.type_ref.as_ref().unwrap().clone();
        match hint {
            TypeCoercionHint::NoCoercion => type_ref,
            TypeCoercionHint::Any => match type_ref {
                TypeRef::Indeterminate(possibilities) => {
                    add_error!(
                        errors,
                        self.span(),
                        IndeterminateType(possibilities.clone())
                    );
                    TypeRef::Unknown
                }
                x => x,
            },
            TypeCoercionHint::Specific(desired) => {
                // TODO: fully implement
                match type_ref {
                    TypeRef::Indeterminate(possibilities) => {
                        for possibility in possibilities {
                            if possibility.type_ref != desired {
                                for span in possibility.spans {
                                    add_error!(errors, span, TypeMismatch(possibility.type_ref.clone(), desired.clone()))
                                }
                            }
                        }
                        TypeRef::Unknown
                    }
                    other => {
                        if other != desired {
                            add_error!(errors, self.value_span(), TypeMismatch(other.clone(), desired));
                            TypeRef::Unknown
                        } else {
                            other
                        }
                    }
                }


            }
        }
    }
}