use crate::source_map::{SourceCollection, Span};
use crate::tokenizer::TokenType::*;
use icu::properties::sets;
use paste::paste;
use std::fmt::{Debug, Formatter};

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

            if !grapheme.chars().all(|c| sets::white_space().contains(c)) {
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

        "-" => Minus,

        ";" => Semicolon,
        "," => Comma,
        ":" => Colon,

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
        "->" => DashArrow,
        "::" => PathSeparator,
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
            // unclear if this is even possible in unicode
            return None;
        }
        self.current += 1;

        while !self.is_end() {
            let str = self.source_collection.get(self.current);
            if !str.chars().all(|c| Self::is_continue(c)) {
                break;
            }
            self.current += 1;
        }

        let span = (start..self.current).into();

        let token_type = match self.source_collection.get(span) {
            "_" => Discard,
            "use" => Use,
            "mod" => Mod,
            "enum" => Enum,
            "as" => As,
            "fn" => Fn,
            "pub" => Pub,
            _ => Identifier,
        };

        Some(Token {
            token_type: token_type,
            span: span,
        })
    }

    fn match_integer(&mut self) -> Option<Token> {
        if self.is_end() {
            return None;
        }

        let start = self.current;

        let str = self.source_collection.get(start);
        let mut chars = str.chars();
        let first = chars.next().expect("grapheme is empty");

        if !matches!(first, '0'..='9') {
            return None;
        }
        if !chars.all(|c| Self::is_continue(c)) {
            // unclear if this is even possible in unicode
            return None;
        }
        self.current += 1;

        while !self.is_end() {
            let str = self.source_collection.get(self.current);
            if !str.chars().all(|c| Self::is_continue(c)) {
                break;
            }
            self.current += 1;
        }

        let span = (start..self.current).into();

        let str = self.source_collection.get(span);

        let mut token_type = if str.starts_with("0b") {
            BinInteger
        } else if str.starts_with("0o") {
            OctInteger
        } else if str.starts_with("0x") {
            HexInteger
        } else {
            DecInteger
        };

        if token_type != DecInteger {
            if str[2..].chars().all(|c| c == '_') {
                token_type = Unknown;
            }
        }

        let token_type = match token_type {
            BinInteger if str[2..].chars().all(|c| matches!(c, '0'..='1'|'_')) => BinInteger,
            OctInteger if str[2..].chars().all(|c| matches!(c, '0'..='7'|'_')) => OctInteger,
            HexInteger if str[2..].chars().all(|c| matches!(c, '0'..='9'|'a'..='f'|'A'..='F'|'_')) => HexInteger,
            DecInteger if str.chars().all(|c| matches!(c, '0'..='9'|'_')) => DecInteger,
            _ => Unknown,
        };

        Some(Token {
            token_type,
            span,
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

        if let Some(token) = self.match_integer() {
            return Some(token);
        }

        self.match_unknown()
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Default)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn unknown() -> Self {
        Token {
            token_type: Unknown,
            span: Span::empty(),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}[{:?}]", self.token_type, self.span)
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Default)]
pub enum TokenType {
    Identifier, // any identifier
    Discard,    // _

    Use,  // use
    As,   // as
    Mod,  // mod
    Enum, // enum
    Fn,   // fn
    Pub,  // pub

    DecInteger, // any decimal integer number, no minus, underscores allowed, leading zeroes allowed
    HexInteger, // any hex integer number prefixed with 0x, no minus, underscores allowed, leading zeroes allowed
    OctInteger, // any octal integer number prefixed with 0o, no minus, underscores allowed, leading zeroes allowed
    BinInteger, // any binary integer number prefixed with 0b, no minus, underscores allowed, leading zeroes allowed

    Minus, // -

    Equals,     // =
    EqualArrow, // =>
    DashArrow,  // ->

    Semicolon, // ;
    Comma,     // ,

    Colon,         // :
    PathSeparator, // ::

    DoubleEquals, // ==

    ArithmeticShiftRight, // >>>

    OpenCurly,   // {
    CloseCurly,  // }
    OpenParen,   // (
    CloseParen,  // )
    OpenSquare,  // [
    CloseSquare, // ]
    OpenAngle,   // <
    CloseAngle,  // >

    LessThan,    // < (not emitted by the tokenizer)
    GreaterThan, // > (not emiited by the tokenizer)

    #[default]
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
        empty: "" -> [],
        identifier: "foo" -> [Identifier],
        two_identifiers: "foo bar" -> [Identifier, Identifier],
        identifier_underscore: "foo_bar" -> [Identifier],
        identifier_start_underscore: "_foo_bar" -> [Identifier],
        identifier_number: "foo123" -> [Identifier],
        number_identifier: "1foo" -> [Unknown],
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
        identifier_leading_underscore: "_foo" -> [Identifier],
        identifier_leading_underscore_number: "_123" -> [Identifier],
        hiragana: "あ" -> [Identifier],
        h: "ℍello" -> [Identifier],
        hairspace: "foo bar" -> [Identifier, Identifier],
        linebreak: "foo\nbar" -> [Identifier, Identifier],
        linebreak_crlf: "foo\r\nbar" -> [Identifier, Identifier],

        continues_with_id_continue: "foo·bar" -> [Identifier],
        starts_with_id_continue: "·foo" -> [Unknown, Identifier],
        illegal_start: "①a23" -> [Unknown, Identifier],

        discard: "_" -> [Discard],

        use: "use" -> [Use],
        as: "as" -> [As],
        mod: "mod" -> [Mod],
        enum: "enum" -> [Enum],
        fn: "fn" -> [Fn],
        pub: "pub" -> [Pub],

        equals: "=" -> [Equals],
        minus: "-" -> [Minus],

        open_paren: "(" -> [OpenParen],
        close_paren: ")" -> [CloseParen],
        open_curly: "{" -> [OpenCurly],
        close_curly: "}" -> [CloseCurly],
        open_square: "[" -> [OpenSquare],
        close_square: "]" -> [CloseSquare],
        open_angle: "<" -> [OpenAngle],
        close_angle: ">" -> [CloseAngle],

        equal_arrow: "=>" -> [EqualArrow],
        dash_arrow: "->" -> [DashArrow],
        double_equals: "==" -> [DoubleEquals],

        semicolon: ";" -> [Semicolon],
        comma: "," -> [Comma],

        colon: ":" -> [Colon],
        path_separator: "::" -> [PathSeparator],

        asr: ">>>" -> [ArithmeticShiftRight],

        two_equals: "= =" -> [Equals, Equals],
        leading_whitespace: " = ==" -> [Equals, DoubleEquals],
        double_equal_equal_arrow_asr: "===>>>>" -> [DoubleEquals, EqualArrow, ArithmeticShiftRight],

        pathsep_colon: ":::" -> [PathSeparator, Colon],
        double_equals_equals: "===" -> [DoubleEquals, Equals],

        decimal_integer: "1234567890" -> [DecInteger],
        decimal_integer_leading_zero : "0123456789" -> [DecInteger],
        decimal_integer_with_underscores: "1_2_3" -> [DecInteger],
        decimal_integer_with_repeated_underscores: "12____3" -> [DecInteger],
        decimal_integer_with_trailing_underscore: "123_" -> [DecInteger],

        invalid_dec_integer_wrong_digits_1: "123abc456" -> [Unknown],
        invalid_dec_integer_wrong_digits_2: "789abc" -> [Unknown],

        hex_integer: "0x0123456789abcdef" -> [HexInteger],
        hex_integer_with_underscores: "0x1_a_3" -> [HexInteger],
        hex_integer_with_repeated_underscores: "0x1a___3" -> [HexInteger],
        hex_integer_with_trailing_underscore: "0x1a3_" -> [HexInteger],

        invalid_hex_integer_only_underscore: "0x_" -> [Unknown],
        invalid_hex_integer_only_prefix: "0x" -> [Unknown],
        invalid_hex_integer_wrong_digits_1: "0xasdf" -> [Unknown],
        invalid_hex_integer_wrong_digits_2: "0x12asdf34" -> [Unknown],

        oct_integer: "0o01234567" -> [OctInteger],
        oct_integer_with_underscores: "0o1_7_3" -> [OctInteger],
        oct_integer_with_repeated_underscores: "0o17___3" -> [OctInteger],
        oct_integer_with_trailing_underscore: "0o173_" -> [OctInteger],

        invalid_oct_integer_only_underscore: "0o_" -> [Unknown],
        invalid_oct_integer_only_prefix: "0o" -> [Unknown],
        invalid_oct_integer_wrong_digits_1: "0o5678" -> [Unknown],
        invalid_oct_integer_wrong_digits_2: "0o56a9b" -> [Unknown],

        bin_integer: "0b01011010" -> [BinInteger],
        bin_integer_with_underscores: "0b0000_1111_0000" -> [BinInteger],
        bin_integer_with_repeated_underscores: "0b1__0___1" -> [BinInteger],
        bin_integer_with_trailing_underscore: "0b101_" -> [BinInteger],

        invalid_bin_integer_only_underscore: "0b_" -> [Unknown],
        invalid_bin_integer_only_prefix: "0b" -> [Unknown],
        invalid_bin_integer_wrong_digits_1: "0b1234" -> [Unknown],
        invalid_bin_integer_wrong_digits_2: "0b001200" -> [Unknown],

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
