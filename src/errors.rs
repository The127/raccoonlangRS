use crate::ast::expressions::binary::BinaryOperator;
use crate::ast::path::Path;
use crate::ast::typing::TypeRef;
use crate::source_map::{HasSpan, SourceCollection, Span};
use crate::tokenizer::TokenType;
use owo_colors::OwoColorize;
use std::fmt::{Display, Formatter};
use std::io::{Stderr, Write};
use ustr::Ustr;

pub struct Errors {
    errors: Vec<Error>,
}

#[cfg(test)]
impl Errors {
    pub fn assert_empty(&self) {
        //TODO: print errors
        let errors = self.get_errors();
        assert!(
            errors.is_empty(),
            "expected errors to be empty, but got {} errors. First error: {:?}",
            errors.len(),
            errors[0]
        )
    }

    pub fn assert_count(&self, error_count: usize) {
        //TODO: print errors
        assert_eq!(self.get_errors().len(), error_count)
    }
}

impl Errors {
    pub fn new() -> Self {
        Errors { errors: vec![] }
    }

    pub fn get_errors(&self) -> &Vec<Error> {
        &self.errors
    }

    pub fn add<S: Into<Span>>(
        &mut self,
        kind: ErrorKind,
        span: S,
        file: &'static str,
        line: u32,
    ) {
        self.errors.push(Error {
            kind,
            span_: span.into(),
            #[cfg(feature = "error-sources")]
            source_file: file,
            #[cfg(feature = "error-sources")]
            source_line: line,
        });
    }

    pub fn merge(&mut self, mut other: Errors) {
        self.errors.append(&mut other.errors);
    }

    #[cfg(test)]
    pub fn has_error_at<S: Into<Span>>(&self, span: S, kind: ErrorKind) -> bool {
        let span = span.into();
        self.errors
            .iter()
            .any(|e| e.span() == span && e.kind == kind)
    }
}

#[macro_export]
macro_rules! add_error {
    ($errors:ident, $span:expr, $($kind:tt)*) => {
        $errors.add(crate::errors::ErrorKind:: $($kind)*, $span, file!(), line!())
    };
}


#[derive(Debug, Eq, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    span_: Span,
    #[cfg(feature = "error-sources")]
    source_file: &'static str,
    #[cfg(feature = "error-sources")]
    source_line: u32,
}

impl HasSpan for Error {
    fn span(&self) -> Span {
        self.span_
    }
}

impl Error {
    pub fn printable(&self, sources: &SourceCollection) -> PrintableError {
        let location = sources.get_location(self.span_.start());
        PrintableError {
            code: self.kind.code(),
            name: self.kind.name(),
            file: location.file,
            line: location.line,
            column: location.column,
            #[cfg(feature = "error-sources")]
            source_file: self.source_file,
            #[cfg(feature = "error-sources")]
            source_line: self.source_line,
        }
    }
}

pub struct PrintableError {
    pub code: &'static str,
    pub name: &'static str,
    pub file: Ustr,
    pub line: usize,
    pub column: usize,
    // TODO: more details

    #[cfg(feature = "error-sources")]
    pub source_file: &'static str,
    #[cfg(feature = "error-sources")]
    pub source_line: u32,
}

impl PrintableError {
    pub(crate) fn print(&self, mut out: impl Write) -> std::io::Result<()> {
        let error_red = format!("[{}] Error:", self.code);
        write!(
            out,
            "{} {} in {}:{}:{}\n",
            error_red.red(),
            self.name,
            self.file,
            self.line,
            self.column
        )?;
        #[cfg(feature = "error-sources")]
        write!(
            out,
            "{} created in {}:{}\n",
            str::repeat(" ", error_red.len()),
            self.source_file,
            self.source_line,
        )?;

        Ok(())
    }
}

macro_rules! define_errorkind {
    (@pattern $type_name:ident $name:ident) => {
        $type_name :: $name
    };
    (@pattern $type_name:ident $name:ident ($($type_:ty),+)) => {
        $type_name :: $name ($(define_errorkind!(@discard $type_)),+)
    };
    (@discard $x:tt) => {
        _
    };
    ($type_name:ident, {$($name:ident $( ( $($args:tt)* ) )? : $code:ident $string_name:literal),*$(,)?}) => {
        #[derive(Debug, Eq, PartialEq)]
        pub enum $type_name {
            $(
                $name $(($($args)*))?,
            )*
        }

        impl $type_name {
            pub fn code(&self) -> &'static str {
                match self {
                    $(
                        define_errorkind!(@pattern $type_name $name $(($($args)*))?) => stringify!($code),
                    )*
                }
            }
            pub fn name(&self) -> &'static str {
                match self {
                    $(
                        define_errorkind!(@pattern $type_name $name $(($($args)*))?) => $string_name,
                    )*
                }
            }
        }
    };
}

define_errorkind!(ErrorKind, {
    MissingSemicolon: E01 "Missing semicolon",
    MissingUsePath: E02 "Missing use path",
    UnexpectedToken(TokenType): E03 "Unexpected token",
    MissingUseAliasName: E04 "Missing use alias name",
    MissingComma: E05 "Missing comma",
    MissingMultiUseName: E06 "Missing multi-use name",
    MissingModulePath: E07 "Missing module path",
    MissingDeclarationName: E08 "Missing declaration name",
    MissingFunctionParameterList: E09 "Missing function parameter list",
    MissingFunctionBody: E10 "Missing function body",
    MissingReturnType: E11 "Missing return type",
    MissingFunctionParameterType: E12 "Missing function parameter type",
    MissingColon: E13 "Missing colon",
    MissingOperand: E14 "Missing operand",
    AmbiguousComparisonExpression(Span): E15 "Ambiguous comparison expression",
    MissingLetDeclarationValue: E16 "Missing let declaration value",
    BinaryOperationInvalidTypes(BinaryOperator, TypeRef, TypeRef): E17 "Invalid operand types for binary operator",
    UnknownVariable(Path): E18 "Unknown variable",
    UnknownType(Path): E19 "Unknown type",
    TypeMismatch(TypeRef, TypeRef): E20 "Type mismatch",
    IndeterminateType(Vec<TypeRef>): E21 "Indeterminate type",
    MissingStructBody: E22 "Missing struct body",
    MissingStructMemberType: E23 "Missing struct member type",
});

impl ErrorKind {
    pub fn get_extra_details(&self) -> () {
        match self {
            ErrorKind::BinaryOperationInvalidTypes(a, b, c) => {}
            _ => (),
        }
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
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
        let f = file!();
        add_error!(error_manager, 23, MissingSemicolon);
        let errors = error_manager.get_errors();

        // assert
        assert_matches!(errors[..], [
            Error {
                kind: ErrorKind::MissingSemicolon,
                span_: Span(23, 24),
                ..
            }
        ]);
    }
}
