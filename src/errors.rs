use crate::source_map::Span;
use crate::tokenizer::TokenType;

pub struct Errors {
    errors: Vec<Error>,
}

impl Errors {
    pub fn new () -> Self {
        Errors { errors: vec![] }
    }

    pub fn get_errors(&self) -> &Vec<Error> {
        &self.errors
    }

    pub fn add(&mut self, kind: ErrorKind, location: Span) {
        self.errors.push(Error {
            kind,
            location,
        })
    }

    #[cfg(test)]
    pub fn has_error_at(&self, span: Span, kind: ErrorKind) -> bool {
        self.errors.iter().any(|e| e.location == span && e.kind == kind)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub location: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ErrorKind {
    MissingSemicolon,
    MissingUsePath,
    UnexpectedToken(TokenType),
    MissingUseAliasName,
    MissingComma,
    MissingMultiUseName,
    MissingModulePath,
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_errors() {
        // arrange
        let mut error_manager = Errors::new();

        // act
        let errors = error_manager.get_errors();

        // assert
        assert_eq!(errors, &vec![]);
    }

    #[test]
    fn single_error() {
        // arrange
        let mut error_manager = Errors::new();

        // act
        error_manager.add(ErrorKind::MissingSemicolon, (23..23).into());
        let errors = error_manager.get_errors();

        // assert
        assert_eq!(errors, &vec![
            Error {
                kind: ErrorKind::MissingSemicolon,
                location: (23..23).into(),
            }
        ]);
    }
}