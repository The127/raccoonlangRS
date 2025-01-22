pub mod type_;
pub mod ir;

use ustr::{Ustr, UstrMap};
use crate::ast::path::Path;

#[derive(Debug)]
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

        if let Some(value) = self.values.get(&path.parts[0]) {
            Some(value)
        } else {
            if let Some(parent) = self.parent {
                parent.lookup(path)
            }else{
                None
            }
        }
    }

    pub fn insert(&mut self, name: Ustr, value: T) {
        let existed = self.values.insert(name, value).is_some();
        assert!(!existed);
    }

    pub fn nested(&self) -> Scope<T> {
        Scope {
            parent: Some(self),
            values: UstrMap::default(),
        }
    }
}


#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use super::*;
    use crate::ast::typing::BuiltinType;
    use parameterized::{ide, parameterized};
    use ustr::ustr;

    ide!();
    #[parameterized(params = {
        ("foo", Some(1)),
        ("bar", Some(2)),
        ("qux", None),
    })]
    fn lookup(params: (&str, Option<i32>)) {
        let (name, expected_type) = params;
        // arrange
        let scope = Scope::from(&[
            (ustr("foo"), 1),
            (ustr("bar"), 2),
        ]);

        // act
        let got_type = scope.lookup(&Path::name(name));

        // assert
        assert_eq!(got_type, expected_type.as_ref());
    }

    #[parameterized(params = {
        ("foo", Some(1)),
        ("bar", Some(2)),
        ("asdf", Some(3)),
        ("qux", None),
    })]
    fn nested_lookup(params: (&str, Option<i32>)) {
        let (name, expected_type) = params;
        // arrange
        let parent_scope = Scope::from(&[
            (ustr("asdf"), 3),
            (ustr("bar"), 4),
        ]);
        let mut scope = Scope::from(&[
            (ustr("foo"), 1),
            (ustr("bar"), 2),
        ]);
        scope.parent = Some(&parent_scope);

        // act
        let got_type = scope.lookup(&Path::name(name));

        // assert
        assert_eq!(got_type, expected_type.as_ref());
    }


    #[test]
    fn nested() {
        // arrange
        let parent_scope = Scope::<i32>::new();

        // act
        let scope = parent_scope.nested();

        // assert
        assert_matches!(scope.parent, Some(p) => {
            assert_eq!(p as *const _, &parent_scope as *const _); // we care about referential equality
        });
        assert!(scope.values.is_empty());
    }

    #[test]
    fn insert() {
        // arrange
        let mut scope = Scope::new();

        // act
        scope.insert(ustr("foo"), 1);

        // assert
        assert_eq!(scope.values.len(), 1);
        assert_eq!(scope.values.get(&ustr("foo")), Some(&1));
    }

    #[test]
    #[should_panic]
    fn duplicate_insert_panics() {
        // arrange
        let mut scope = Scope::new();

        // act
        scope.insert(ustr("foo"), 1);
        scope.insert(ustr("foo"), 2);
    }
}