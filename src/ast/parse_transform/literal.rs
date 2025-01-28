use crate::ast::parse_transform::Expression;
use crate::errors::Errors;
use crate::parser::literal_expression_node::{
    BooleanLiteralNode, LiteralExpressionNode, NumberLiteralNode,
};
use crate::source_map::{HasSpan, SourceCollection};
use crate::tokenizer::TokenType;

pub fn transform_literal_expression(
    node: &LiteralExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    match node {
        LiteralExpressionNode::Number(x) => transform_literal_number_expression(x, sources),
        LiteralExpressionNode::Boolean(x) => transform_literal_bool_expression(x, sources),
    }
}

fn transform_literal_number_expression(
    node: &NumberLiteralNode,
    sources: &SourceCollection,
) -> Expression {
    let prefix_length = match node.number.token_type {
        TokenType::DecInteger => 0,
        TokenType::HexInteger => 2,
        TokenType::OctInteger => 2,
        TokenType::BinInteger => 2,
        TokenType::Float => 0,
        _ => unreachable!(),
    };

    let radix = match node.number.token_type {
        TokenType::DecInteger => 10,
        TokenType::HexInteger => 16,
        TokenType::OctInteger => 8,
        TokenType::BinInteger => 2,
        TokenType::Float => 10,
        _ => unreachable!(),
    };

    let str = sources.get_str(node.number.span().sub(prefix_length..));

    let num_string = match node.number.token_type {
        TokenType::DecInteger => str
            .chars()
            .take_while(|c| matches!(c, '0'..='9' | '_'))
            .collect::<String>(),
        TokenType::OctInteger => str
            .chars()
            .take_while(|c| matches!(c, '0'..='7' | '_'))
            .collect::<String>(),
        TokenType::HexInteger => str
            .chars()
            .take_while(|c| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'))
            .collect::<String>(),
        TokenType::BinInteger => str
            .chars()
            .take_while(|c| matches!(c, '0'..='1' | '_'))
            .collect::<String>(),
        TokenType::Float => str
            .chars()
            .take_while(|c| matches!(c, '0'..='9' | '.' | '_'))
            .collect::<String>(),
        _ => unreachable!(),
    };

    let suffix = &str[num_string.len()..];

    let mut num_string = num_string.chars().filter(|c| *c != '_').collect::<String>();

    if node.negative {
        num_string.insert(0, '-');
    }

    if suffix == "i32"
        || (suffix == ""
            && matches!(
                node.number.token_type,
                TokenType::DecInteger
                    | TokenType::BinInteger
                    | TokenType::OctInteger
                    | TokenType::HexInteger
            ))
    {
        let res = i32::from_str_radix(num_string.as_str(), radix);
        let val = res.unwrap();
        return Expression::i32_literal(node.span(), val);
    } else if suffix == "u32" {
        let res = u32::from_str_radix(num_string.as_str(), radix);
        let val = res.unwrap();
        return Expression::u32_literal(node.span(), val);
    } else if suffix == "f32" || (suffix == "" && node.number.token_type == TokenType::Float) {
        let res = num_string.parse();
        let val = res.unwrap();
        return Expression::f32_literal(node.span(), val);
    } else if suffix != "" {
        // TODO: error
        return Expression::unknown();
    }

    unreachable!();
}

fn transform_literal_bool_expression(
    node: &BooleanLiteralNode,
    sources: &SourceCollection,
) -> Expression {
    match node.value.token_type {
        TokenType::True => Expression::bool_literal(node.span(), true),
        TokenType::False => Expression::bool_literal(node.span(), false),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::parse_transform::transform_expression;
    use crate::errors::Errors;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{
        BooleanLiteralNode, LiteralExpressionNode, NumberLiteralNode,
    };
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType;
    use crate::tokenizer::TokenType::{BinInteger, DecInteger, Float, HexInteger, OctInteger};
    use parameterized::parameterized;

    #[parameterized(params = {
        ("0", false, 0),
        ("1", false, 1),
        ("_123", false, 123),
        ("2_147_483_647", false, 2_147_483_647),
        ("0", true, 0),
        ("1__23", true, -123),
        ("2_147_483_648_", true, -2_147_483_648),
        ("123i32", false, 123),
    })]
    fn transform_decimal_integer_literal(params: (&str, bool, i32)) {
        let (input, negative, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(DecInteger:span), negative),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::i32_literal(span, output));
    }

    #[parameterized(params = {
        ("0b0", false, 0b0),
        ("0b1", false, 0b1),
        ("0b1i32", false, 0b1),
        ("0b_00110101_11010100", false, 0b_00110101_11010100),
        ("0b111", true, -0b111),
    })]
    fn transform_binary_integer_literal(params: (&str, bool, i32)) {
        let (input, negative, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(BinInteger:span), negative),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::i32_literal(span, output));
    }

    #[parameterized(params = {
        ("0o0", false, 0o0),
        ("0o1", false, 0o1),
        ("0o_137", false, 0o_137),
        ("0o17_777_777_777", false, 0o17_777_777_777),
        ("0o123", true, -0o123),
        ("0o123i32", false, 0o123)
    })]
    fn transform_octal_integer_literal(params: (&str, bool, i32)) {
        let (input, negative, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(OctInteger:span), negative),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::i32_literal(span, output));
    }

    #[parameterized(params = {
        ("0x00", false, 0x0),
        ("0x01", false, 0x1),
        ("0x12_34_", false, 0x1234),
        ("0x7FFF_FFFF", false, 0x7FFF_FFFF),
        ("0x1234", true, -0x1234),
        ("0x123i32", false, 0x123),
    })]
    fn transform_hexadecimal_integer_literal(params: (&str, bool, i32)) {
        let (input, negative, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(HexInteger:span), negative),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::i32_literal(span, output));
    }

    #[parameterized(params = {
        ("12.34", false, 12.34),
        ("0.23", false, 0.23),
        ("1_.2", false, 1.2),
        ("1._2_", false, 1.2),
        ("1.0f32", false, 1.0),
        ("1.23", true, -1.23),
    })]
    fn transform_float_literal(params: (&str, bool, f32)) {
        let (input, negative, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(Float:span), negative),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::f32_literal(span, output));
    }

    #[test]
    fn transform_int_literal_with_float_suffix() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("123_f32");

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(DecInteger:span), false),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::f32_literal(span, 123.0));
    }

    #[test]
    fn transform_integer_invalid_suffix() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("123foo");

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(span, test_token!(DecInteger:span), false),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::unknown());
    }

    #[parameterized(params = {
        ("true", TokenType::True, true),
        ("false", TokenType::False, false),
    })]
    fn transform_bool_literal(params: (&str, TokenType, bool)) {
        let (input, token_type, expected_value) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Boolean(
            BooleanLiteralNode::new(span, test_token!(token_type:span)),
        ));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::bool_literal(span, expected_value));
    }
}
