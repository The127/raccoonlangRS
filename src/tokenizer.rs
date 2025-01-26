use crate::source_map::{HasSpan, SourceCollection, Span};
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
                let str = self.source_collection.get_str(current..(current + $count));

                let token_type = match str {
                    $(
                        $input => $type,
                    )*
                    _ => return None,
                };

                self.current += $count;

                Some(Token::new(current..(current + $count), token_type))
            }
        }
    }
}

impl<'a> Tokenizer<'a> {

    fn input_starts_with(&self, expected: &str) -> bool {
        let expected_len = expected.len();
        if self.current + expected_len > self.end {
            return false;
        }

        let input_str = self.source_collection.get_str(self.current .. (self.current + expected_len));

        input_str == expected
    }

    fn skip_line_comment(&mut self) -> bool {
        if self.input_starts_with("//") {
            self.current += 2;
            while !self.is_end() {
                let grapheme = self
                    .source_collection
                    .get_str(self.current);

                self.current += 1;
                if (grapheme == "\n") || (grapheme == "\r") || (grapheme == "\r\n") {
                    return true;
                }
            }
        }
        false
    }

    fn skip_multiline_comment(&mut self) -> bool {
        if self.input_starts_with("/*") {
            self.current += 2;
            while !self.is_end() {
                if self.input_starts_with("*/") {
                    self.current += 2;
                    return true;
                }

                self.current += 1;
            }
        }
        false
    }

    fn skip_whitespace(&mut self) -> bool {
        let mut consumed = false;
        while !self.is_end() {
            let grapheme = self
                .source_collection
                .get_str(self.current);

            if !grapheme.chars().all(|c| sets::white_space().contains(c)) {
                break;
            }

            self.current += 1;
            consumed = true;
        }

        consumed
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
        "+" => Plus,

        "/" => Slash,
        "*" => Asterisk,

        ";" => Semicolon,
        "," => Comma,
        ":" => Colon,
        "." => Dot,

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
        "!=" => NotEquals,
        "<=" => LessOrEquals,
        ">=" => GreaterOrEquals,
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

    fn match_doc_comment(&mut self) -> Option<Token> {
        if !self.input_starts_with("///") {
            return None;
        }
        if self.input_starts_with("////") {
            return None;
        }

        let start = self.current;

        while !self.is_end() {
            let grapheme = self
                .source_collection
                .get_str(self.current);

            self.current += 1;
            if (grapheme == "\n") || (grapheme == "\r") || (grapheme == "\r\n") {
                break;
            }
        }

        return Some(Token::new(start..self.current, DocComment));
    }

    fn match_identifier(&mut self) -> Option<Token> {
        if self.is_end() {
            return None;
        }

        let start = self.current;

        let str = self.source_collection.get_str(start);
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
            let str = self.source_collection.get_str(self.current);
            if !str.chars().all(|c| Self::is_continue(c)) {
                break;
            }
            self.current += 1;
        }

        let span: Span = (start..self.current).into();

        let token_type = match self.source_collection.get_str(span) {
            "_" => Discard,
            "use" => Use,
            "mod" => Mod,
            "enum" => Enum,
            "struct" => Struct,
            "as" => As,
            "fn" => Fn,
            "pub" => Pub,
            "if" => If,
            "else" => Else,
            "let" => Let,
            "true" => True,
            "false" => False,
            "with" => With,
            _ => Identifier,
        };

        Some(Token::new(span, token_type))
    }

    fn match_number(&mut self) -> Option<Token> {
        if self.is_end() {
            return None;
        }

        let start = self.current;

        let str = self.source_collection.get_str(start);
        let mut chars = str.chars();
        let first = chars.next().expect("grapheme is empty");

        if !matches!(first, '0'..='9') {
            return None;
        }
        assert!(chars.next().is_none()); // no further chars in a grapheme cluster after digit
        self.current += 1;

        let mut dot_occured = false;
        let mut id_occured = false;
        let mut last_was_dot = false;

        while !self.is_end() {
            let str = self.source_collection.get_str(self.current);

            if !str.chars().all(|c| matches!(c, '0'..='9' | '_')) {
                if str == "." {
                    if dot_occured || id_occured {
                        break;
                    } else {
                        dot_occured = true;
                        last_was_dot = true;
                    }
                } else {
                    if last_was_dot {
                        dot_occured = false;
                        self.current -= 1;
                        break;
                    }

                    if !str.chars().all(Self::is_continue) {
                        break;
                    }

                    last_was_dot = false;
                    id_occured = true;
                }
            } else {
                last_was_dot = false;
            }

            self.current += 1;
        }

        let span: Span = (start..self.current).into();

        let str = self.source_collection.get_str(span);

        let mut token_type = if str.starts_with("0b") {
            BinInteger
        } else if str.starts_with("0o") {
            OctInteger
        } else if str.starts_with("0x") {
            HexInteger
        } else if dot_occured {
            Float
        } else {
            DecInteger
        };

        Some(Token::new(span, token_type))
    }

    fn match_unknown(&mut self) -> Option<Token> {
        if self.is_end() {
            return None;
        }

        let start = self.current;
        self.current += 1;

        Some(Token::new(start, Unknown))
    }
}

pub fn tokenize(span: Span, source_collection: &SourceCollection) -> Tokenizer {
    Tokenizer {
        source_collection: source_collection,
        current: span.start(),
        end: span.end(),
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {

        loop {
            if let Some(token) = self.match_doc_comment() {
                return Some(token);
            }

            if self.skip_whitespace()  { continue };

            if self.skip_line_comment() { continue };

            if self.skip_multiline_comment() { continue };

            break;
        }


        if let Some(token) = self.match_identifier() {
            return Some(token);
        }

        if let Some(token) = self.match_number() {
            return Some(token);
        }

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

#[derive(Eq, PartialEq, Copy, Clone, Default)]
pub struct Token {
    pub token_type: TokenType,
    span_: Span,
}

impl Token {
    pub fn unknown() -> Self {
        Token {
            token_type: Unknown,
            span_: Span::empty(),
        }
    }

    pub fn new<S: Into<Span>>(span: S, token_type: TokenType) -> Self {
        Token {
            token_type,
            span_: span.into(),
        }
    }
}

impl HasSpan for Token {
    fn span(&self) -> Span {
        self.span_
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}[{:?}]", self.token_type, self.span_)
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
    Struct, // enum
    Fn,   // fn
    Pub,  // pub
    Let,  // let
    With, // with

    If,   // if
    Else, // else

    True,  // true
    False, // false

    DecInteger, // any decimal integer number, no minus, underscores allowed, leading zeroes allowed
    HexInteger, // any hex integer number prefixed with 0x, no minus, underscores allowed, leading zeroes allowed
    OctInteger, // any octal integer number prefixed with 0o, no minus, underscores allowed, leading zeroes allowed
    BinInteger, // any binary integer number prefixed with 0b, no minus, underscores allowed, leading zeroes allowed

    Float, // any floating point number

    Minus, // -
    Plus,  // +

    Slash,    // /
    Asterisk, // *

    Equals,     // =
    EqualArrow, // =>
    DashArrow,  // ->

    Semicolon, // ;
    Comma,     // ,
    Dot,       // .

    Colon,         // :
    PathSeparator, // ::

    DoubleEquals,    // ==
    NotEquals,       // !=
    LessOrEquals,    // <=
    GreaterOrEquals, // >=

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

    DocComment, // doc comment (starts with ///)

    #[default]
    Unknown, // anything that does not match
}

#[cfg(test)]
mod test {
    use crate::test_tokens;
    use crate::tokenizer::*;

    #[test]
    fn is_end_before_end() {
        // arrange
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content("abc");

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
        let span = source_collection.load_content("abc");

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
        let span = source_collection.load_content("abc");

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
        let span = source_collection.load_content("ab");

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
        let span = source_collection.load_content("abcdef");

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
        let span = source_collection.load_content("abcdef");

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
        let span = source_collection.load_content("abc");

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
        let span = source_collection.load_content("abc");

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
        let span = source_collection.load_content("abcdef");

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
                let span = source_collection.load_content($input);

                let tokenizer = tokenize(span, &source_collection);

                // act
                let tokens = tokenizer.map(|x| x.token_type).collect::<Vec<_>>();

                // assert
                assert_eq!(tokens, vec! $expected, "{}", $input);
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
        number_dot_identifier: "123.foo" -> [DecInteger, Dot, Identifier],
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
        fn: "fn" -> [Fn],
        pub: "pub" -> [Pub],
        enum: "enum" -> [Enum],
        struct: "struct" -> [Struct],
        with: "with" -> [With],

        if: "if" -> [If],
        else: "else" -> [Else],
        let: "let" -> [Let],
        true: "true" -> [True],
        false: "false" -> [False],



        equals: "=" -> [Equals],
        minus: "-" -> [Minus],
        plus: "+" -> [Plus],

        slash: "/" -> [Slash],
        asterisk: "*" -> [Asterisk],

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
        not_equals: "!=" -> [NotEquals],
        less_or_equals: "<=" -> [LessOrEquals],
        greater_or_equals: ">=" -> [GreaterOrEquals],

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
        decimal_integer_with_suffix: "123foo" -> [DecInteger],

        hex_integer: "0x0123456789abcdef" -> [HexInteger],
        hex_integer_with_underscores: "0x1_a_3" -> [HexInteger],
        hex_integer_with_repeated_underscores: "0x1a___3" -> [HexInteger],
        hex_integer_with_trailing_underscore: "0x1a3_" -> [HexInteger],
        hex_integer_with_suffix: "0x12i32" -> [HexInteger],

        invalid_hex_integer_only_underscore: "0x_" -> [HexInteger],
        invalid_hex_integer_only_prefix: "0x" -> [HexInteger],
        invalid_hex_integer_only_prefix_and_suffix: "0xhjkl" -> [HexInteger],
        invalid_hex_integer_only_prefix_underscore_suffix: "0x_hjkl" -> [HexInteger],

        oct_integer: "0o01234567" -> [OctInteger],
        oct_integer_with_underscores: "0o1_7_3" -> [OctInteger],
        oct_integer_with_repeated_underscores: "0o17___3" -> [OctInteger],
        oct_integer_with_trailing_underscore: "0o173_" -> [OctInteger],
        oct_integer_with_suffix: "0o01234567890abc" -> [OctInteger],


        invalid_oct_integer_only_underscore: "0o_" -> [OctInteger],
        invalid_oct_integer_only_prefix: "0o" -> [OctInteger],
        invalid_oct_integer_only_prefix_and_suffix: "0o89a" -> [OctInteger],
        invalid_oct_integer_only_prefix_underscore_suffix: "0o_89a" -> [OctInteger],

        bin_integer: "0b01011010" -> [BinInteger],
        bin_integer_with_underscores: "0b0000_1111_0000" -> [BinInteger],
        bin_integer_with_repeated_underscores: "0b1__0___1" -> [BinInteger],
        bin_integer_with_trailing_underscore: "0b101_" -> [BinInteger],
        bin_integer_with_suffix: "0b1010234abc" -> [BinInteger],

        invalid_bin_integer_only_underscore: "0b_" -> [BinInteger],
        invalid_bin_integer_only_prefix: "0b" -> [BinInteger],
        invalid_bin_integer_only_prefix_and_suffix: "0b345" -> [BinInteger],
        invalid_bin_integer_only_prefix_underscore_suffix: "0b_345" -> [BinInteger],

        float_1: "0.123" -> [Float],
        float_2: ".123" -> [Dot, DecInteger],
        float_3: "0._0" -> [Float],
        float_4: "._0" -> [Dot, Identifier],
        float_5: "0." -> [Float],
        float_6: "." -> [Dot],
        float_7: ".." -> [Dot, Dot],
        float_8: "0.." -> [Float, Dot],
        float_9: ".foo" -> [Dot, Identifier],
        float_10: "0.foo" -> [DecInteger, Dot, Identifier],
        float_11: ".0a" -> [Dot, DecInteger],
        float_12: "0.0a" -> [Float],
        float_13: ".0x20" -> [Dot, HexInteger],
        float_14: "0.0x20" -> [Float],
        float_15: "._" -> [Dot, Discard],
        float_16: "0._" -> [Float],
        float_17: "0.0foo" -> [Float],
        float_18: ".0foo" -> [Dot, DecInteger],
        float_19: "." -> [Dot],
        float_20: ".1." -> [Dot, Float],
        float_21: "12.34.56" -> [Float, Dot, DecInteger],
        float_22: "12.34.56.78" -> [Float, Dot, Float],

        int_with_float_suffix: "123f32" -> [DecInteger],


        empty_line_comment: "//" -> [],
        line_comment: "foo\nbar // hello world" -> [Identifier, Identifier],
        line_comment_first_line: "// foo" -> [],
        line_comment_with_following_line: "// foo \nbar" -> [Identifier],
        line_comment_end: "foo // bar" -> [Identifier],
        multiline_comment: "foo/* hello */bar" -> [Identifier, Identifier],
        multiline_comment_span_multiple_lines: "foo /* hello \n world */ bar" -> [Identifier, Identifier],

        doc_comment: "foo /// bar" -> [Identifier, DocComment],
        empty_doc_comment: "///" -> [DocComment],
        doc_comment_start_of_line: "/// foo bar" -> [DocComment],
        doc_comment_without_space: "///foo bar" -> [DocComment],
        doc_comment_after_whitespace: "    /// hello world" -> [DocComment],
        commented_out_doc_comment_1: "// ///" -> [],
        not_a_doc_comment_1: "////" -> [],
        not_a_doc_comment_2: "//// foo" -> [],
        not_a_doc_comment_3: "/////" -> [],
        not_a_doc_comment_4: "///// bar" -> [],
        doc_comment_later_line: "foo\nbar\n/// hello world" -> [Identifier, Identifier, DocComment],
        multiple_doc_comments: "/// foo\n/// bar\n/// foo bar" -> [DocComment, DocComment, DocComment],

        unknown: "§" -> [Unknown],
    }

    /// foo
    /// bar
    /// hello
    /// world
    #[test]
    fn tokenize_spans() {
        let mut source_collection = SourceCollection::new();
        let span = source_collection.load_content(" =  = ");

        let tokenizer = tokenize(span, &source_collection);

        // act
        let tokens = tokenizer.collect::<Vec<_>>();

        // assert
        assert_eq!(tokens, test_tokens!(Equals:1..2, Equals:4..5))
    }
}
