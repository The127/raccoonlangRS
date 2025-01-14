pub mod type_;
pub mod ir;

use crate::ast::typing::TypeRef;
use ustr::{Ustr, UstrMap};
use crate::ast::path::Path;

pub struct Scope<'a, T> {
    parent: Option<&'a Scope<'a, T>>,
    values: UstrMap<T>,
}

impl<'a, T> Scope<'a, T> {
    pub fn new() -> Self {
        Self {
            parent: None,
            values: UstrMap::default(),
        }
    }

    pub fn from(values: &[(Ustr, T)]) -> Self where T : Clone {
        let mut scope = Self {
            parent: None,
            values: UstrMap::default(),
        };
        for (name, value) in values {
            scope.values.insert(*name, value.clone());
        }
        scope
    }

    pub fn lookup(&self, path: &Path) -> Option<&T> {
        assert_eq!(path.parts.len(), 1);
        assert!(!path.rooted);
        let value = self.values.get(&path.parts[0]);
        if value.is_none() {
            if let Some(parent) = self.parent {
                return parent.lookup(path);
            }
        }
        value
    }

    pub fn insert(&mut self, name: Ustr, value: T) {
        todo!()
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::typing::BuiltinType;
    use parameterized::{ide, parameterized};
    use ustr::ustr;

    ide!();
    #[parameterized(params = {
        ("foo", Some(TypeRef::Unknown)),
        ("bar", Some(TypeRef::Builtin(BuiltinType::I32))),
        ("qux", None),
    })]
    fn lookup(params: (&str, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let scope = Scope::from(&[
            (ustr("foo"), TypeRef::Unknown),
            (ustr("bar"), TypeRef::Builtin(BuiltinType::I32)),
        ]);

        // act
        let got_type = scope.lookup(&Path::name(name));

        // assert
        assert_eq!(got_type, expected_type.as_ref());
    }

    #[parameterized(params = {
        ("foo", Some(TypeRef::Unknown)),
        ("bar", Some(TypeRef::Builtin(BuiltinType::I32))),
        ("asdf", Some(TypeRef::Builtin(BuiltinType::Unit))),
        ("qux", None),
    })]
    fn nested_lookup(params: (&str, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let parent_scope = Scope::from(&[
            (ustr("bar"), TypeRef::Builtin(BuiltinType::Bool)),
            (ustr("asdf"), TypeRef::Builtin(BuiltinType::Unit)),
        ]);
        let mut scope = Scope::from(&[
            (ustr("foo"), TypeRef::Unknown),
            (ustr("bar"), TypeRef::Builtin(BuiltinType::I32)),
        ]);
        scope.parent = Some(&parent_scope);

        // act
        let got_type = scope.lookup(&Path::name(name));

        // assert
        assert_eq!(got_type, expected_type.as_ref());
    }
}