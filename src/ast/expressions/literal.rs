use crate::ast::expressions::Expression;
use crate::parser::literal_expression_node::LiteralExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};
use crate::tokenizer::TokenType;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LiteralExpression {
    pub(super) span_: Span,
    pub value: LiteralValue,
}

impl HasSpan for LiteralExpression {
    fn span(&self) -> Span {
        self.span_
    }
}


#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LiteralValue {
    Integer(i32),
    Boolean(bool),
}


pub fn transform_literal_expression(
    node: &LiteralExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    match node {
        LiteralExpressionNode::Integer(x) => {
            if x.number.token_type == TokenType::DecInteger {
                let str = sources.get_str(x.number.span());
                let mut string = String::with_capacity(str.len() + 1);
                if x.negative {
                    string.push('-');
                }
                string.push_str(str);
                string.retain(|c| c != '_');

                let res = string.parse();
                let val: i32 = res.unwrap(); // TODO: test error

                return Expression::int_literal(node.span(), val);
            }

            let (prefix, radix) = match x.number.token_type {
                TokenType::HexInteger => ("0x", 16),
                TokenType::OctInteger => ("0o", 8),
                TokenType::BinInteger => ("0b", 2),
                _ => unreachable!(),
            };
            let str = sources.get_str(x.number.span());
            debug_assert!(str.starts_with(prefix));
            let mut string = str[prefix.len()..].to_string();
            string.retain(|c| c != '_');

            let res = u32::from_str_radix(&string, radix);
            let val: u32 = res.unwrap(); // TODO: test error
            Expression::int_literal(node.span(), val as i32)
        }
        LiteralExpressionNode::Boolean(x) => {
            match x.value.token_type {
                TokenType::True => Expression::bool_literal(x.span(), true),
                TokenType::False => Expression::bool_literal(x.span(), false),
                _ => unreachable!()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use parameterized::parameterized;
    use crate::ast::expressions::{transform_expression, Expression, ExpressionKind};
    use crate::ast::expressions::literal::{LiteralExpression, LiteralValue};
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{BooleanLiteralNode, IntegerLiteralNode, LiteralExpressionNode};
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType;
    use crate::tokenizer::TokenType::{BinInteger, DecInteger, HexInteger, OctInteger};

    #[parameterized(params = {
        ("0", false, 0),
        ("1", false, 1),
        ("_123", false, 123),
        ("2_147_483_647", false, 2_147_483_647),
        ("0", true, 0),
        ("1__23", true, -123),
        ("2_147_483_648_", true, -2_147_483_648),
    })]
    fn transform_decimal_integer_literal(params: (&str, bool, i32)) {
        let (input, negative, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(DecInteger:span), negative),
        ));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Literal(LiteralExpression {
                    span_: span,
                    value: LiteralValue::Integer(output),
                }),
                type_ref: None,
            }
        );
    }

    #[parameterized(params = {
        ("0b0", 0),
        ("0b1", 1),
        ("0b_00110101_11010100", 13780),
        ("0b01111111_11111111___11111111_11111111", 2_147_483_647),
        ("0b11111111_11111111_11111111_11111111__", -1),
        ("0b10000000_00000000_00000000_00000000_", -2_147_483_648),
    })]
    fn transform_binary_integer_literal(params: (&str, i32)) {
        let (input, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(BinInteger:span), false),
        ));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Literal(LiteralExpression {
                    span_: span,
                    value: LiteralValue::Integer(output),
                }),
                type_ref: None,
            }
        );
    }

    #[parameterized(params = {
        ("0o0", 0),
        ("0o1", 1),
        ("0o_137", 95),
        ("0o17_777_777_777", 2_147_483_647),
        ("0o20_000_000_000_", -2_147_483_648),
        ("0o37__777_777_777", -1),
    })]
    fn transform_octal_integer_literal(params: (&str, i32)) {
        let (input, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(OctInteger:span), false),
        ));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Literal(LiteralExpression {
                    span_: span,
                    value: LiteralValue::Integer(output),
                }),
                type_ref: None,
            }
        );
    }

    #[parameterized(params = {
        ("0x00", 0),
        ("0x01", 1),
        ("0x12_34_", 4660),
        ("0x7FFF_FFFF", 2_147_483_647),
        ("0x8000__0000", -2_147_483_648),
        ("0x_FFFF_FFFF", -1),
    })]
    fn transform_hexadecimal_integer_literal(params: (&str, i32)) {
        let (input, output) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(HexInteger:span), false),
        ));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Literal(LiteralExpression {
                    span_: span,
                    value: LiteralValue::Integer(output),
                }),
                type_ref: None,
            }
        );
    }

    #[parameterized(params = {
        ("true", TokenType::True, true),
        ("false", TokenType::False, false),
    })]
    fn transform_bool_literal(params: (&str, TokenType, bool)) {
        let (input, token_type, expected_value) = params;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(input);

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Boolean(
            BooleanLiteralNode::new(span, test_token!(token_type:span)),
        ));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Literal(LiteralExpression {
                    span_: span,
                    value: LiteralValue::Boolean(expected_value),
                }),
                type_ref: None,
            }
        );
    }
}