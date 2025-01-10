use std::sync::atomic::{AtomicUsize, Ordering};


const NAMESPACE_BUILTIN: u8 = 0;
const NAMESPACE_LOCAL: u8 = 1;
const NAMESPACE_GLOBAL: u8 = 2;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct VarId(u8, usize);


#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct TypeId(u8, usize);



impl VarId {
    pub fn local(id: usize) -> Self {
        Self(NAMESPACE_LOCAL, id)
    }

    pub fn global(id: usize) -> Self {
        Self(NAMESPACE_GLOBAL, id)
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

    pub fn i32() -> Self {
        Self(NAMESPACE_BUILTIN, 1)
    }
}