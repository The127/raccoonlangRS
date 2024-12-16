use crate::source_map::{SourceCollection, Span};
use crate::tokenizer::TokenType::*;
use std::ops::Range;
use paste::paste;

pub struct Tokenizer<'a> {
    source_collection: &'a SourceCollection,
    current: usize,
    end: usize,
}


macro_rules! match_symbolic_tokens {
        ($count:literal, [$($input:literal => $type:ident,)*]) => {
        paste! {
            fn [<match_symbol_ $count>](&mut self) -> Option<Token> {
                if !self.has_at_least($count) {
                    return None;
                }

                let current = self.current;
                let str = self.source_collection.get(current..(current + $count));

                let token_type = match str {
                    $(
                        $input => $type,
                    )*
                    _ => return None,
                };

                self.current += $count;

                Some(Token {
                    token_type: token_type,
                    span: (current..(current + $count)).into(),
                })
            }
        }
    }
}

impl<'a> Tokenizer<'a> {
    fn skip_white_space(&mut self) {
        while !self.is_end() {
            match self.source_collection.get(self.current..self.current + 1) {
                " " => self.current += 1,
                _ => break,
            }
        }
    }

    fn is_end(&self) -> bool {
        self.current >= self.end
    }

    fn has_at_least(&self, count: usize) -> bool {
        self.current + count <= self.end
    }

    match_symbolic_tokens!(1, [
        "=" => Equals,
    ]);

    match_symbolic_tokens!(2, [
        "==" => DoubleEquals,
        "=>" => EqualArrow,
    ]);

    match_symbolic_tokens!(3, [
        ">>>" => ArithmeticShiftRight,
    ]);

    fn match_unknown(&mut self) -> Option<Token> {
        if self.is_end() {
            return None;
        }

        let start = self.current;
        self.current += 1;

        Some(Token {
            token_type: Unknown,
            span: Span {
                start: start,
                end: start + 1,
            },
        })
    }
}

pub fn tokenize(span: Span, source_collection: &SourceCollection) -> Tokenizer {
    Tokenizer {
        source_collection: source_collection,
        current: span.start,
        end: span.end,
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_white_space();

        if let Some(token) = self.match_symbol_3() {
            return Some(token);
        }

        if let Some(token) = self.match_symbol_2() {
            return Some(token);
        }

        if let Some(token) = self.match_symbol_1() {
            return Some(token);
        }

        self.match_unknown()
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct Token {
    token_type: TokenType,
    span: Span,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    Equals,       // =
    EqualArrow,   // =>
    DoubleEquals, // ==
    ArithmeticShiftRight, // >>>

    Unknown, // anything that does not match
}

#[cfg(test)]
mod test {
    use crate::tokenizer::*;

    #[test]
    fn is_end_before_end() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abc".to_string());

        let tokenizer = tokenize(span, &source_collection);

        // act
        let result = tokenizer.is_end();

        // assert
        assert!(!result);
    }

    #[test]
    fn is_end_at_end() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abc".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 3;

        // act
        let result = tokenizer.is_end();

        // assert
        assert!(result);
    }

    #[test]
    fn is_end_past_end() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abc".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 4;

        // act
        let result = tokenizer.is_end();

        // assert
        assert!(result);
    }

    #[test]
    fn at_least_one_enough() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("ab".to_string());

        let tokenizer = tokenize(span, &source_collection);

        // act
        let result = tokenizer.has_at_least(1);

        // assert
        assert!(result);
    }

    #[test]
    fn at_least_enough() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abcdef".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 3;

        // act
        let result = tokenizer.has_at_least(2);

        // assert
        assert!(result);
    }

    #[test]
    fn at_least_exactly() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abcdef".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 4;

        // act
        let result = tokenizer.has_at_least(2);

        // assert
        assert!(result);
    }

    #[test]
    fn at_least_one_exactly() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abc".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 2;

        // act
        let result = tokenizer.has_at_least(1);

        // assert
        assert!(result);
    }

    #[test]
    fn at_least_one_too_many() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abc".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 3;

        // act
        let result = tokenizer.has_at_least(1);

        // assert
        assert!(!result);
    }

    #[test]
    fn at_least_too_many() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abcdef".to_string());

        let mut tokenizer = tokenize(span, &source_collection);
        tokenizer.current += 5;

        // act
        let result = tokenizer.has_at_least(2);

        // assert
        assert!(!result);
    }

    macro_rules! token_tests {
        ($($name:ident: $input:literal -> $expected:tt,)*) => {
        $(
        paste! {
            #[test]
            fn [<tokenize_ $name >] () {
                // arrange
                let mut source_collection = SourceCollection::new();
                let span = source_collection.load_content($input.to_string());

                let tokenizer = tokenize(span, &source_collection);

                // act
                let tokens = tokenizer.map(|x| x.token_type).collect::<Vec<_>>();

                // assert
                assert_eq!(tokens, vec! $expected)
            }
        }
        )*
        }
    }

    token_tests! {
        unknown: "§" -> [Unknown],
        equals: "=" -> [Equals],
        two_equals: "= =" -> [Equals, Equals],
        leading_whitespace: " = =" -> [Equals, Equals],
        equal_arrow: "=>" -> [EqualArrow],
        double_equals: "==" -> [DoubleEquals],
        asr: ">>>" -> [ArithmeticShiftRight],
        double_equal_equal_arrow_asr: "===>>>>" -> [DoubleEquals, EqualArrow, ArithmeticShiftRight],
    }

    #[test]
    fn tokenize_spans() {
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content(" =  = ".to_string());

        let tokenizer = tokenize(span, &source_collection);

        // act
        let tokens = tokenizer.collect::<Vec<_>>();

        // assert
        assert_eq!(
            tokens,
            vec![
                Token {
                    token_type: Equals,
                    span: (1..2).into(),
                },
                Token {
                    token_type: Equals,
                    span: (4..5).into(),
                }
            ]
        )
    }
}
