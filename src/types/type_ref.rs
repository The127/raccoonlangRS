use crate::parser::Spanned;
use crate::refs::ImmutableRef;
use crate::source_map::{HasSpan, Span};
use crate::util::vecs_equal_ignore_order;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
pub struct StructType {
    pub members: Vec<StructTypeMember>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructTypeMember {
    pub name: Ustr,
    pub type_ref: TypeRef,
}

impl StructTypeMember {
    pub fn new(name: Ustr, type_ref: TypeRef) -> Self {
        Self { name, type_ref }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
pub struct FunctionType {
    pub params: Vec<FunctionTypeParam>,
    pub return_: TypeRef,
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
pub struct FunctionTypeParam{
    pub name: Option<Ustr>,
    pub type_ref: TypeRef,
}

impl FunctionTypeParam {
    pub fn new(name: Option<Ustr>, type_ref: TypeRef) -> Self {
        Self {
            name,
            type_ref
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
pub struct FunctionOverloadGroup {
    pub overloads: Vec<ImmutableRef<FunctionType>>
}

impl FunctionOverloadGroup {
    // TODO
}

#[derive(Debug, Clone, Hash, Default)]
pub enum TypeRef {
    #[default]
    Unknown,
    Builtin(BuiltinType),
    Tuple(TupleType),
    Struct(ImmutableRef<StructType>),
    Function(ImmutableRef<FunctionType>),
    FunctionOverloadGroup(FunctionOverloadGroup),
    Indeterminate(Vec<IndeterminateTypePossibility>),
}

impl PartialEq<Self> for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeRef::Unknown, TypeRef::Unknown) => true,
            (TypeRef::Builtin(x1), TypeRef::Builtin(x2)) => x1 == x2,
            (TypeRef::Tuple(x1), TypeRef::Tuple(x2)) => x1 == x2,
            (TypeRef::Struct(x1), TypeRef::Struct(x2)) => x1 == x2,
            (TypeRef::Function(x1), TypeRef::Function(x2)) => x1 == x2,
            (TypeRef::Indeterminate(x1), TypeRef::Indeterminate(x2)) => {
                vecs_equal_ignore_order(x1, x2)
            }
            (_, _) => false,
        }
    }
}

impl Eq for TypeRef {}

#[derive(Debug, Clone, Hash, Default)]
pub struct IndeterminateTypePossibility {
    pub type_ref: TypeRef,
    pub spans: Vec<Span>,
}

impl PartialEq for IndeterminateTypePossibility {
    fn eq(&self, other: &Self) -> bool {
        self.type_ref == other.type_ref && vecs_equal_ignore_order(&self.spans, &other.spans)
    }
}

impl Eq for IndeterminateTypePossibility {}

impl IndeterminateTypePossibility {
    pub fn new(type_ref: TypeRef, sources: Vec<Span>) -> Self {
        Self {
            type_ref,
            spans: sources,
        }
    }
}

impl TypeRef {
    pub const fn unit() -> Self {
        Self::Builtin(BuiltinType::Unit)
    }

    pub const fn bool() -> Self {
        Self::Builtin(BuiltinType::Bool)
    }

    pub const fn i32() -> Self {
        Self::Builtin(BuiltinType::I32)
    }

    pub const fn u32() -> Self {
        Self::Builtin(BuiltinType::U32)
    }

    pub const fn f32() -> Self {
        Self::Builtin(BuiltinType::F32)
    }

    pub const fn tuple(fields: Vec<TypeRef>) -> Self {
        Self::Tuple(TupleType { fields })
    }

    pub fn merge_indeterminate(t1: Spanned<Self>, t2: Spanned<Self>) -> Self {
        match (&t1.value, &t2.value) {
            (TypeRef::Indeterminate(p1), TypeRef::Indeterminate(p2)) => {
                TypeRef::Indeterminate(Self::merge_indeterminate_vecs(p1, p2))
            }
            (TypeRef::Indeterminate(p), other) => {
                TypeRef::Indeterminate(Self::merge_indeterminate_vecs(
                    p,
                    &vec![IndeterminateTypePossibility::new(
                        other.clone(),
                        vec![t2.span()],
                    )],
                ))
            }
            (this, TypeRef::Indeterminate(p)) => {
                TypeRef::Indeterminate(Self::merge_indeterminate_vecs(
                    &vec![IndeterminateTypePossibility::new(
                        this.clone(),
                        vec![t1.span()],
                    )],
                    p,
                ))
            }
            (this, other) if this == other => this.clone(),
            (this, other) => TypeRef::Indeterminate(vec![
                IndeterminateTypePossibility::new(this.clone(), vec![t1.span()]),
                IndeterminateTypePossibility::new(other.clone(), vec![t2.span()]),
            ]),
        }
    }

    fn merge_indeterminate_vecs(
        v1: &Vec<IndeterminateTypePossibility>,
        v2: &Vec<IndeterminateTypePossibility>,
    ) -> Vec<IndeterminateTypePossibility> {
        let mut possibilities = HashMap::new();
        for vec in [v1, v2] {
            for possibility in vec {
                let entry = possibilities.entry(&possibility.type_ref).or_insert(vec![]);
                for span in &possibility.spans {
                    entry.push(*span);
                }
            }
        }
        possibilities
            .into_iter()
            .map(|(type_ref, spans)| IndeterminateTypePossibility::new(type_ref.clone(), spans))
            .collect()
    }

    pub fn get_member_type(&self, name: Ustr) -> Option<TypeRef> {
        match self {
            TypeRef::Struct(s) => {
                let borrow = s.borrow();
                for member in &borrow.members {
                    if member.name == name {
                        return Some(member.type_ref.clone());
                    }
                }
                None
            },
            _ => todo!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum BuiltinType {
    Unit,
    Bool,
    I32,
    U32,
    F32,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TupleType {
    pub fields: Vec<TypeRef>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ToSpanned;
    use crate::source_map::Span;
    use crate::types::type_ref::TypeRef;
    use parameterized::{ide, parameterized};

    ide!();

    #[parameterized(params = {
        (TypeRef::Unknown, TypeRef::Unknown, TypeRef::Unknown),
        (TypeRef::Unknown, TypeRef::unit(), TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::Unknown, vec![Span(3,4)]),
            IndeterminateTypePossibility::new(TypeRef::unit(), vec![Span(17,18)]),
        ])),
        (TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::unit(), vec![Span(1,2)]),
            IndeterminateTypePossibility::new(TypeRef::i32(), vec![Span(2,3)]),
        ]), TypeRef::Unknown, TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::unit(), vec![Span(1,2)]),
            IndeterminateTypePossibility::new(TypeRef::i32(), vec![Span(2,3)]),
            IndeterminateTypePossibility::new(TypeRef::Unknown, vec![Span(17,18)]),
        ])),
        (TypeRef::unit(), TypeRef::Unknown, TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::unit(), vec![Span(3,4)]),
            IndeterminateTypePossibility::new(TypeRef::Unknown, vec![Span(17,18)]),
        ])),
        (TypeRef::unit(), TypeRef::unit(), TypeRef::unit()),
        (TypeRef::i32(), TypeRef::u32(), TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::i32(), vec![Span(3,4)]),
            IndeterminateTypePossibility::new(TypeRef::u32(), vec![Span(17,18)]),
        ])),
        (TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::unit(), vec![Span(1,2)]),
            IndeterminateTypePossibility::new(TypeRef::i32(), vec![Span(2,3)]),
        ]), TypeRef::u32(), TypeRef::Indeterminate(vec![
            IndeterminateTypePossibility::new(TypeRef::unit(), vec![Span(1,2)]),
            IndeterminateTypePossibility::new(TypeRef::i32(), vec![Span(2,3)]),
            IndeterminateTypePossibility::new(TypeRef::u32(), vec![Span(17,18)]),
        ])),
    })]
    fn merge_indeterminate(params: (TypeRef, TypeRef, TypeRef)) {
        let (left, right, expected) = params;
        // arrange

        // act
        let result = TypeRef::merge_indeterminate(left.spanned(3..4), right.spanned(17..18));

        // assert
        assert_eq!(result, expected);
    }
}
