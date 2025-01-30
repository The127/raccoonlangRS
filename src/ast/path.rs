use std::fmt::{Debug, Display, Formatter};
use ustr::Ustr;

#[derive(Eq, PartialEq, Clone)]
pub struct Path {
    pub parts: Vec<Ustr>,
    pub rooted: bool,
}

impl Path {
    pub fn new(parts: Vec<Ustr>, rooted: bool) -> Self {
        assert!(parts.len() > 0);
        Self { parts, rooted }
    }

    #[cfg(test)]
    pub fn name<T: Into<Ustr>>(name: T) -> Self {
        Self {
            parts: vec![name.into()],
            rooted: false,
        }
    }
}

impl Debug for Path {
    #[mutants::skip]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Path({})", self)
    }
}

impl Display for Path {
    #[mutants::skip]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for part in &self.parts {
            if self.rooted || !first {
                write!(f, "::")?;
            }
            write!(f, "{}", part)?;
            first = false;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parameterized::{ide, parameterized};
    use ustr::ustr;

    ide!();

    #[parameterized(params = {
        (vec!["foo", "bar"], false, "Path(foo::bar)"),
        (vec!["foo", "bar"], true, "Path(::foo::bar)"),
        (vec!["foo"], false, "Path(foo)"),
        (vec!["foo"], true, "Path(::foo)"),
    })]
    fn debug(params: (Vec<&str>, bool, &str)) {
        let (parts, rooted, expected) = params;
        // arrange
        let path = Path::new(parts.into_iter().map(|x| ustr(x)).collect(), rooted);

        // act
        let result = format!("{:?}", path);

        // assert
        assert_eq!(result, expected);
    }

    #[parameterized(params = {
        (vec!["foo", "bar"], false, "foo::bar"),
        (vec!["foo", "bar"], true, "::foo::bar"),
        (vec!["foo"], false, "foo"),
        (vec!["foo"], true, "::foo"),
    })]
    fn display(params: (Vec<&str>, bool, &str)) {
        let (parts, rooted, expected) = params;
        // arrange
        let path = Path::new(parts.into_iter().map(|x| ustr(x)).collect(), rooted);

        // act
        let result = format!("{}", path);

        // assert
        assert_eq!(result, expected);
    }
}