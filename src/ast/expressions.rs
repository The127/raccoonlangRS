use crate::parser::add_expression_node::AddExpressionNode;
use crate::parser::block_expression_node::BlockExpressionNode;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::literal_expression_node::LiteralExpressionNode;
use crate::source_map::{SourceCollection, Span};
use crate::tokenizer::TokenType;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Block(BlockExpression),
    Literal(LiteralExpression),
    Add(AddExpression),
    Unknown(UnknownExpression),
}

impl Expression {
    pub fn unknown() -> Self {
        Expression::Unknown(UnknownExpression {
            span: Span::empty(),
        })
    }

    pub fn block<S: Into<Span>>(span: S, value: Option<Expression>) -> Self {
        Expression::Block(BlockExpression {
            span: span.into(),
            value: value.map(|x| Box::new(x)),
        })
    }

    pub fn int_literal<S: Into<Span>>(span: S, value: i32) -> Self {
        Expression::Literal(LiteralExpression {
            span: span.into(),
            value: LiteralValue::Integer(value),
        })
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct BlockExpression {
    pub span: Span,
    pub value: Option<Box<Expression>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LiteralExpression {
    pub span: Span,
    pub value: LiteralValue,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LiteralValue {
    Integer(i32),
}

#[derive(Debug, Eq, PartialEq)]
pub struct AddExpression {
    pub span: Span,
    pub left: Box<Expression>,
    pub follows: Vec<AddExpressionFollow>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum AddExpressionOperator {
    OpPlus,
    OpMinus,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AddExpressionFollow {
    operator: AddExpressionOperator,
    operand: Option<Box<Expression>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct UnknownExpression {
    pub span: Span,
}

pub fn transform_expression(node: &ExpressionNode, sources: &SourceCollection) -> Expression {
    match node {
        ExpressionNode::Unknown => Expression::unknown(),
        ExpressionNode::Literal(x) => transform_literal_expression(x, sources),
        ExpressionNode::Block(x) => transform_block_expression(x, sources),
        ExpressionNode::Add(x) => transform_plus_expression(x, sources),
        ExpressionNode::Compare(x) => todo!(),
    }
}

pub fn transform_literal_expression(
    node: &LiteralExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    match node {
        LiteralExpressionNode::Integer(x) => {
            if x.number.token_type == TokenType::DecInteger {
                let str = sources.get_str(x.number.span);
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
            let str = sources.get_str(x.number.span);
            debug_assert!(str.starts_with(prefix));
            let mut string = str[prefix.len()..].to_string();
            string.retain(|c| c != '_');

            let res = u32::from_str_radix(&string, radix);
            let val: u32 = res.unwrap(); // TODO: test error
            Expression::int_literal(node.span(), val as i32)
        }
    }
}

pub fn transform_block_expression(
    node: &BlockExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::block(
        node.span,
        node.value
            .as_ref()
            .map(|n| transform_expression(n, sources)),
    )
}

pub fn transform_plus_expression(
    node: &AddExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    let mut result = AddExpression{
        span: node.span,
        left: Box::new(transform_expression(node.left.as_ref(), sources)),
        follows: vec![],
    };

    for follow in node.follows.iter() {
        result.follows.push(AddExpressionFollow {
            operator: match &follow.operator.token_type {
                TokenType::Plus => AddExpressionOperator::OpPlus,
                TokenType::Minus => AddExpressionOperator::OpMinus,
                _ => unreachable!(),
            },
            operand: match &follow.operand {
                Some(operand) => Some(Box::new(transform_expression(operand.as_ref(), sources))),
                None => None,
            },
        });
    }

    Expression::Add(result)
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::AddExpressionOperator::OpPlus;
    use crate::ast::expressions::{transform_expression, AddExpression, AddExpressionFollow, Expression};
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::{BinInteger, DecInteger, HexInteger, OctInteger, Plus};
    use parameterized::parameterized;

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

        let expr_node =
            ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span,
                number: test_token!(DecInteger:span),
                negative: negative,
            }));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(expr, Expression::int_literal(span, output));
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

        let expr_node =
            ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span,
                number: test_token!(BinInteger:span),
                negative: false,
            }));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(expr, Expression::int_literal(span, output));
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

        let expr_node =
            ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span,
                number: test_token!(OctInteger:span),
                negative: false,
            }));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(expr, Expression::int_literal(span, output));
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

        let expr_node =
            ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode {
                span,
                number: test_token!(HexInteger:span),
                negative: false,
            }));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(expr, Expression::int_literal(span, output));
    }

    #[test]
    fn transform_empty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ }");

        let block_node = ExpressionNode::Block(BlockExpressionNode { span, value: None });

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(expr, Expression::block(span, None));
    }

    #[test]
    fn transform_nonempty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ 123 }");
        let num_span = ((span.start + 2)..(span.end - 2)).into();

        let block_node = ExpressionNode::Block(BlockExpressionNode {
            span,
            value: Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode {
                    span: num_span,
                    number: test_token!(DecInteger:num_span),
                    negative: false,
                }),
            ))),
        });

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::block(span, Some(Expression::int_literal(num_span, 123)))
        );
    }

    #[test]
    fn transform_nested_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ { } }");
        let inner_span = ((span.start + 2)..(span.end - 2)).into();

        let block_node = ExpressionNode::Block(BlockExpressionNode {
            span,
            value: Some(Box::new(ExpressionNode::Block(BlockExpressionNode {
                span: inner_span,
                value: None,
            }))),
        });

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::block(span, Some(Expression::block(inner_span, None)))
        );
    }

    #[test]
    fn transform_add() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 + 2");

        let add_node = ExpressionNode::Add(AddExpressionNode{
            span,
            left: Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode{
                span: 0.into(),
                number: test_token!(DecInteger:0),
                negative: false,
            }))),
            follows: vec![AddExpressionNodeFollow{
                operator: test_token!(Plus:2),
                operand: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(IntegerLiteralNode{
                    span: 4.into(),
                    number: test_token!(DecInteger:4),
                    negative: false,
                })))),
            }],
        });

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::Add(AddExpression{
                span,
                left: Box::new(Expression::int_literal(0, 1)),
                follows: vec![AddExpressionFollow {
                    operator: OpPlus,
                    operand: Some(Box::new(Expression::int_literal(4, 2))),
                }],
            })
        );
    }
}
