use crate::source_map::{SourceCollection, Span};
use crate::tokenizer::TokenType::*;
use std::ops::Range;

pub struct Tokenizer<'a> {
    source_collection: &'a SourceCollection,
    iter: Range<usize>,
}

pub fn tokenize(span: Span, source_collection: &SourceCollection) -> Tokenizer {
    Tokenizer {
        source_collection: source_collection,
        iter: span.into(),
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let start = match self.iter.next() {
                None => return None,
                Some(x) => x
            };
            let str = self.source_collection.get(start..(start + 1));

            let token_type = match str {
                " " => continue,
                "=" => Equals,
                _ => Unknown,
            };

            return Some(Token {
                token_type: token_type,
                span: Span { start: start, end: start + str.len() },
            });
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct Token {
    token_type: TokenType,
    span: Span,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    Equals,         // =
    EqualArrow,     // =>
    DoubleEquals,   // ==

    Unknown,        // anything that does not match
}

#[cfg(test)]
mod test {
    use crate::tokenizer::*;

    macro_rules! token_tests {
        ($($name:ident: $input:literal -> $expected:tt,)*) => {
        $(
            #[test]
            fn $name() {
                // arrange
                let mut source_collection = SourceCollection::new();
                let span = source_collection.load_content($input.to_string());

                let tokenizer = tokenize(span, &source_collection);

                // act
                let tokens = tokenizer.map(|x| x.token_type).collect::<Vec<_>>();

                // assert
                assert_eq!(tokens, vec! $expected)
            }
        )*
        }
    }

    token_tests! {
        tokenize_unknown: "§" -> [Unknown],
        tokenize_equals: "=" -> [Equals],
        tokenize_two_equals: "= =" -> [Equals, Equals],
        tokenize_leading_whitespace: " = =" -> [Equals, Equals],
        // tokenize_equal_arrow: "=>" -> [EqualArrow],
        //tokenize_double_equals: "==" -> [DoubleEquals],
    }

    #[test]
    fn tokenize_spans() {
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content(" =  = ".to_string());

        let tokenizer = tokenize(span, &source_collection);

        // act
        let tokens = tokenizer.collect::<Vec<_>>();

        // assert
        assert_eq!(tokens, vec![Token {
            token_type: Equals,
            span: (1..2).into(),
        }, Token {
            token_type: Equals,
            span: (4..5).into(),
        }])
    }
}
