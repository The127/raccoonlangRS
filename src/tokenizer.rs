use std::fmt::{Debug, Formatter};
use icu::properties::sets;
use crate::source_map::{SourceCollection, Span};
use crate::tokenizer::TokenType::*;
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
            let grapheme = self.source_collection.get(self.current..self.current + 1);

            let mut chars = grapheme.chars();
            let first = chars.next().expect("grapheme is empty");
            if let Some(_) = chars.next() {
                break; // if the grapheme has more than one character then it is not whitespace
            }
            if !sets::white_space().contains(first) {
                break;
            }
            self.current += 1;
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

        "(" => OpenParen,
        ")" => CloseParen,
        "{" => OpenCurly,
        "}" => CloseCurly,
        "[" => OpenSquare,
        "]" => CloseSquare,
        "<" => OpenAngle,
        ">" => CloseAngle,
    ]);

    match_symbolic_tokens!(2, [
        "==" => DoubleEquals,
        "=>" => EqualArrow,
    ]);

    match_symbolic_tokens!(3, [
        ">>>" => ArithmeticShiftRight,
    ]);

    fn is_continue(c: char) -> bool {
        sets::id_continue().contains(c) || sets::extended_pictographic().contains(c)
    }

    fn is_start(c: char) -> bool {
        c == '_' || sets::id_start().contains(c) || sets::extended_pictographic().contains(c)
    }

    fn match_identifier(&mut self) -> Option<Token> {
        if self.is_end() {
            return None;
        }

        let start = self.current;

        let str = self.source_collection.get(start);
        let mut chars = str.chars();
        let first = chars.next().expect("grapheme is empty");

        if !Self::is_start(first) {
            return None;
        }
        if !chars.all(|c| Self::is_continue(c)) {
            return None;
        }
        self.current += 1;

        while !self.is_end() {
            let str = self.source_collection.get(self.current);
            if !str.chars().all(|c| Self::is_continue(c)) {
                break
            }
            self.current += 1;
        }

        let span = (start..self.current).into();

        let token_type = match self.source_collection.get(span){
            "_" => Discard,
            "use" => Use,
            "mod" => Mod,
            "enum" => Enum,
            _ => Identifier,
        };

        Some(Token {
            token_type: token_type,
            span: span,
        })
    }

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

        if let Some(token) = self.match_identifier() {
            return Some(token);
        }

        self.match_unknown()
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn unknown() -> Self {
        Token {
            token_type: Unknown,
            span: (0..0).into(),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}[{:?}]", self.token_type, self.span)
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {

    Identifier, // any identifier
    Discard,    // _

    Use,            // use
    Mod,            // mod
    Enum,           // enum

    Equals,     // =
    EqualArrow, // =>

    DoubleEquals, // ==

    ArithmeticShiftRight, // >>>

    OpenCurly,      // {
    CloseCurly,     // }
    OpenParen,      // (
    CloseParen,     // )
    OpenSquare,     // [
    CloseSquare,    // ]
    OpenAngle,      // <
    CloseAngle,     // >

    LessThan,       // < (not emitted by the tokenizer)
    GreaterThan,    // > (not emiited by the tokenizer)

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
        identifier: "foo" -> [Identifier],
        two_identifiers: "foo bar" -> [Identifier, Identifier],
        identifier_underscore: "foo_bar" -> [Identifier],
        identifier_start_underscore: "_foo_bar" -> [Identifier],
        identifier_number: "foo123" -> [Identifier],
        number_identifier: "1foo" -> [Unknown, Identifier],
        identifier_start_emoji: "😀🥰" -> [Identifier],
        identifier_joined_emoji: "👷‍♀️" -> [Identifier],
        identifier_single_long_emoji: "🗺" -> [Identifier],
        identifier_mixed_emoji: "foo💃" -> [Identifier],
        identifier_sanskrit: "मांजर" -> [Identifier],
        identifier_tamil: "நி" -> [Identifier],
        identifier_tamil_mixed: "aநி" -> [Identifier],
        identifier_chinese: "王记餐馆" -> [Identifier],
        identifier_hangul: "한글" -> [Identifier],
        identifier_amogus: "ඞ" -> [Identifier],
        hiragana: "あ" -> [Identifier],
        H: "ℍello" -> [Identifier],
        hairspace: "foo bar" -> [Identifier, Identifier],

        continues_with_id_continue: "foo·bar" -> [Identifier],
        starts_with_id_continue: "·foo" -> [Unknown, Identifier],
        illegal_start: "①a23" -> [Unknown, Identifier],

        discard: "_" -> [Discard],

        use: "use" -> [Use],
        mod: "mod" -> [Mod],
        enum: "enum" -> [Enum],

        equals: "=" -> [Equals],

        open_paren: "(" -> [OpenParen],
        close_paren: ")" -> [CloseParen],
        open_curly: "{" -> [OpenCurly],
        close_curly: "}" -> [CloseCurly],
        open_square: "[" -> [OpenSquare],
        close_square: "]" -> [CloseSquare],
        open_angle: "<" -> [OpenAngle],
        close_angle: ">" -> [CloseAngle],

        equal_arrow: "=>" -> [EqualArrow],
        double_equals: "==" -> [DoubleEquals],

        asr: ">>>" -> [ArithmeticShiftRight],

        two_equals: "= =" -> [Equals, Equals],
        leading_whitespace: " = ==" -> [Equals, DoubleEquals],
        double_equal_equal_arrow_asr: "===>>>>" -> [DoubleEquals, EqualArrow, ArithmeticShiftRight],

        unknown: "§" -> [Unknown],
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
