use std::fmt::{Debug, Formatter};


const NAMESPACE_BUILTIN: u8 = 0;
const NAMESPACE_LOCAL: u8 = 1;
const NAMESPACE_GLOBAL: u8 = 2;

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct VarId(u8, usize);


#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct TypeId(u8, usize);



impl VarId {
    pub fn local(id: usize) -> Self {
        Self(NAMESPACE_LOCAL, id)
    }

    pub fn global(id: usize) -> Self {
        Self(NAMESPACE_GLOBAL, id)
    }
}

impl Debug for VarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            NAMESPACE_BUILTIN => write!(f, "Var(builtin:{})", self.1),
            NAMESPACE_LOCAL => write!(f, "Var(local:{})", self.1),
            NAMESPACE_GLOBAL => write!(f, "Var(global:{})", self.1),
            _ => write!(f, "Var({}:{})", self.0, self.1),
        }
    }
}

impl TypeId {
    pub fn local(id: usize) -> Self {
        Self(NAMESPACE_LOCAL, id)
    }

    pub fn global(id: usize) -> Self {
        Self(NAMESPACE_GLOBAL, id)
    }

    pub fn unit() -> Self {
        Self(NAMESPACE_BUILTIN, 0)
    }

    pub fn bool() -> Self {
        Self(NAMESPACE_BUILTIN, 1)
    }

    pub fn i32() -> Self {
        Self(NAMESPACE_BUILTIN, 2)
    }
}

impl Debug for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self == &Self::unit() {
            write!(f, "Type(unit)")
        } else if self == &Self::bool() {
            write!(f, "Type(bool)")
        } else if self == &Self::i32() {
            write!(f, "Type(i32)")
        } else {
            match self.0 {
                NAMESPACE_BUILTIN => write!(f, "Type(builtin:{})", self.1),
                NAMESPACE_LOCAL => write!(f, "Type(local:{})", self.1),
                NAMESPACE_GLOBAL => write!(f, "Type(global:{})", self.1),
                _ => write!(f, "Type({}:{})", self.0, self.1),
            }
        }

    }
}