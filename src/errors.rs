use crate::source_map::{HasSpan, Span};
use crate::tokenizer::{Token, TokenType};

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

    pub fn add<S: Into<Span>>(&mut self, kind: ErrorKind, span: S) {
        self.errors.push(Error {
            kind,
            span_: span.into(),
        })
    }

    pub fn merge(&mut self, mut other: Errors) {
        self.errors.append(&mut other.errors);
    }

    #[cfg(test)]
    pub fn has_error_at<S: Into<Span>>(&self, span: S, kind: ErrorKind) -> bool {
        let span = span.into();
        self.errors.iter().any(|e| e.span() == span && e.kind == kind)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    span_: Span,
}

impl HasSpan for Error {
    fn span(&self) -> Span {
        self.span_
    }
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
    MissingFunctionName,
    MissingFunctionParameterList,
    MissingFunctionBody,
    MissingReturnType,
    MissingFunctionParameterType,
    MissingColon,
    MissingOperand,
    AmbiguousComparisonExpression(Span),
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_errors() {
        // arrange
        let error_manager = Errors::new();

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
        error_manager.add(ErrorKind::MissingSemicolon, 23);
        let errors = error_manager.get_errors();

        // assert
        assert_eq!(errors, &vec![
            Error {
                kind: ErrorKind::MissingSemicolon,
                span_: 23.into(),
            }
        ]);
    }
}