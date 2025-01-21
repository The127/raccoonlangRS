use std::fmt::{Debug, Display, Formatter};


const NAMESPACE_BUILTIN: u8 = 0;

const NAMESPACE_PARAM: u8 = 1;
const NAMESPACE_LOCAL: u8 = 2;
const NAMESPACE_GLOBAL: u8 = 3;

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct VarId(u8, usize);

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct TypeId(u8, usize);

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct SignatureId(usize);

impl VarId {
    pub fn local(id: usize) -> Self {
        Self(NAMESPACE_LOCAL, id)
    }

    pub fn param(id: usize) -> Self {
        Self(NAMESPACE_PARAM, id)
    }

    pub fn global(id: usize) -> Self {
        Self(NAMESPACE_GLOBAL, id)
    }

    pub fn discard() -> Self {
        Self(NAMESPACE_BUILTIN, 0)
    }
}

impl Debug for VarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self == &VarId::discard() {
            write!(f, "Var(discard)")
        } else {
            match self.0 {
                NAMESPACE_BUILTIN => write!(f, "Var(builtin:{})", self.1),
                NAMESPACE_LOCAL => write!(f, "Var(local:{})", self.1),
                NAMESPACE_PARAM => write!(f, "Var(param:{})", self.1),
                NAMESPACE_GLOBAL => write!(f, "Var(global:{})", self.1),
                _ => write!(f, "Var({}:{})", self.0, self.1),
            }
        }
    }
}

impl Display for VarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self == &VarId::discard() {
            write!(f, "discard")
        } else {
            match self.0 {
                NAMESPACE_BUILTIN => write!(f, "builtin_{}", self.1),
                NAMESPACE_LOCAL => write!(f, "local_{}", self.1),
                NAMESPACE_PARAM => write!(f, "param_{}", self.1),
                NAMESPACE_GLOBAL => write!(f, "global_{}", self.1),
                _ => write!(f, "var_{}_{}", self.0, self.1),
            }
        }
    }
}

impl TypeId {
    pub fn new(id: usize) -> Self {
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

    pub fn u32() -> Self {
        Self(NAMESPACE_BUILTIN, 3)
    }

    pub fn f32() -> Self {
        Self(NAMESPACE_BUILTIN, 4)
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
        } else if self == &Self::u32() {
            write!(f, "Type(u32)")
        } else if self == &Self::f32() {
            write!(f, "Type(f32)")
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

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self == &Self::unit() {
            write!(f, "unit")
        } else if self == &Self::bool() {
            write!(f, "bool")
        } else if self == &Self::i32() {
            write!(f, "i32")
        }  else if self == &Self::u32() {
            write!(f, "u32")
        }   else if self == &Self::f32() {
            write!(f, "f32")
        } else {
            match self.0 {
                NAMESPACE_BUILTIN => write!(f, "builtin_{}", self.1),
                NAMESPACE_LOCAL => write!(f, "local_{}", self.1),
                NAMESPACE_GLOBAL => write!(f, "global_{}", self.1),
                _ => write!(f, "type_{}_{}", self.0, self.1),
            }
        }
    }
}

impl SignatureId {
    pub fn new(id: usize) -> Self {
        assert!(id > 0);
        Self(id)
    }

    pub fn empty() -> Self {
        Self(0)
    }
}

impl Debug for SignatureId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self == &Self::empty() {
            write!(f, "Signature(empty)")
        } else {
            write!(f, "Signature({})", self.0)
        }
    }
}

impl Display for SignatureId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self == &Self::empty() {
            write!(f, "_")
        } else {
            write!(f, "sig_{}", self.0)
        }
    }
}