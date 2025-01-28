use std::cell::{Ref, RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct MutableRef<T> {
    value: Rc<RefCell<T>>
}

impl<T> MutableRef<T> {
    pub fn get_immutable(&self) -> ImmutableRef<T> {
        ImmutableRef {
            value: Rc::clone(&self.value)
        }
    }

    pub fn borrow(&self) -> Ref<'_, T> {
        self.value.as_ref().borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.value.as_ref().borrow_mut()
    }
}

impl<T> Clone for MutableRef<T> where T: Clone {
    fn clone(&self) -> Self {
        Self {
            value: Rc::new(RefCell::new(self.value.borrow().clone()))
        }
    }
}


#[derive(Debug)]
pub struct ImmutableRef<T> {
    value: Rc<RefCell<T>>
}

impl<T> ImmutableRef<T> {
    pub fn borrow(&self) -> Ref<'_, T> {
        self.value.as_ref().borrow()
    }
}

impl<T> Clone for ImmutableRef<T> {
    fn clone(&self) -> Self {
        Self {
            value: Rc::clone(&self.value)
        }
    }
}

impl<T> PartialEq for ImmutableRef<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.value, &other.value)
    }
}

impl<T> Eq for ImmutableRef<T> {}

impl<T> Hash for ImmutableRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let raw_ptr = Rc::into_raw(self.value.clone());
        raw_ptr.hash(state);

        // avoid memory leak, see documentation of Rc::into_raw()
        let _ = unsafe { Rc::from_raw(raw_ptr) };
    }
}

