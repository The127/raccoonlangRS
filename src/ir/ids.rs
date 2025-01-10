use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct VarId(usize);

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct TypeId(usize);


static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);
static TYPE_COUNTER: AtomicUsize = AtomicUsize::new(1);

impl VarId {
    pub fn new() -> Self {
        Self(VAR_COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl TypeId {
    pub fn new() -> Self {
        Self(TYPE_COUNTER.fetch_add(1, Ordering::SeqCst))
    }

    pub fn i32() -> Self {
        Self(0)
    }
}