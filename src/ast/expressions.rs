use crate::ast::statement::Statement;
use crate::ast::typing::TypeRef;
use crate::parser::access_expression_node::AccessExpressionNode;
use crate::parser::add_expression_node::AddExpressionNode;
use crate::parser::block_expression_node::BlockExpressionNode;
use crate::parser::compare_expression_node::CompareExpressionNode;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::if_expression_node::IfExpressionNode;
use crate::parser::literal_expression_node::LiteralExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};
use crate::tokenizer::TokenType;
use ustr::Ustr;
use crate::parser::mul_expression_node::MulExpressionNode;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_ref: Option<TypeRef>,
}

impl HasSpan for Expression {
    fn span(&self) -> Span {
        self.kind.span()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionKind {
    Literal(LiteralExpression),
    Access(AccessExpression),
    Block(BlockExpression),
    Binary(BinaryExpression),
    If(IfExpression),
    Unknown(UnknownExpression),
}

impl HasSpan for ExpressionKind {
    fn span(&self) -> Span {
        match self {
            ExpressionKind::Literal(x) => x.span(),
            ExpressionKind::Access(x) => x.span(),
            ExpressionKind::Block(x) => x.span(),
            ExpressionKind::Binary(x) => x.span(),
            ExpressionKind::If(x) => x.span(),
            ExpressionKind::Unknown(x) => x.span(),
        }
    }
}

impl Expression {
    pub fn unknown() -> Self {
        Expression {
            kind: ExpressionKind::Unknown(UnknownExpression {
                span_: Span::empty(),
            }),
            type_ref: None,
        }
    }

    pub fn block<S: Into<Span>>(
        span: S,
        statements: Vec<Statement>,
        value: Option<Expression>,
    ) -> Self {
        Expression {
            kind: ExpressionKind::Block(BlockExpression {
                span_: span.into(),
                statements,
                value: value.map(Box::new),
            }),
            type_ref: None,
        }
    }

    pub fn int_literal<S: Into<Span>>(span: S, value: i32) -> Self {
        Expression {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::Integer(value),
            }),
            type_ref: None,
        }
    }

    pub fn bool_literal<S: Into<Span>>(span: S, value: bool) -> Self {
        Expression {
            kind: ExpressionKind::Literal(LiteralExpression {
                span_: span.into(),
                value: LiteralValue::Boolean(value),
            }),
            type_ref: None,
        }
    }

    pub fn access<S: Into<Span>>(span: S, name: Ustr) -> Self {
        Expression {
            kind: ExpressionKind::Access(AccessExpression {
                span_: span.into(),
                name,
            }),
            type_ref: None,
        }
    }

    pub fn binary<S: Into<Span>>(
        span: S,
        op: BinaryOperator,
        left: Expression,
        right: Expression,
    ) -> Self {
        Expression {
            kind: ExpressionKind::Binary(BinaryExpression {
                span_: span.into(),
                op,
                left: Box::new(left),
                right: Box::new(right),
            }),
            type_ref: None,
        }
    }

    pub fn if_<S: Into<Span>>(
        span: S,
        condition: Expression,
        then: Expression,
        else_: Option<Expression>,
    ) -> Self {
        Expression {
            kind: ExpressionKind::If(IfExpression {
                span_: span.into(),
                condition: Box::new(condition),
                then: Box::new(then),
                else_: else_.map(Box::new),
            }),
            type_ref: None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpression {
    span_: Span,
    pub statements: Vec<Statement>,
    pub value: Option<Box<Expression>>,
}

impl HasSpan for BlockExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LiteralExpression {
    span_: Span,
    pub value: LiteralValue,
}

impl HasSpan for LiteralExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AccessExpression {
    span_: Span,
    pub name: Ustr,
}

impl HasSpan for AccessExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LiteralValue {
    Integer(i32),
    Boolean(bool),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryExpression {
    span_: Span,
    pub op: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Equals,
    NotEquals,
    LessThanOrEquals,
    GreaterThanOrEquals,
    LessThan,
    GreaterThan,
}

impl HasSpan for BinaryExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum AddExpressionOperator {
    OpPlus,
    OpMinus,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AddExpressionFollow {
    pub operator: AddExpressionOperator,
    pub operand: Option<Box<Expression>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression {
    span_: Span,
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Option<Box<Expression>>,
}

impl HasSpan for IfExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CompareExpression {
    span_: Span,
    pub left: Option<Box<Expression>>,
    pub operator: CompareExpressionOperator,
    pub right: Option<Box<Expression>>,
}

impl HasSpan for CompareExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum CompareExpressionOperator {
    OpEquals,
    OpNotEquals,
    OpLessThan,
    OpLessOrEquals,
    OpGreaterThan,
    OpGreaterOrEquals,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnknownExpression {
    span_: Span,
}

impl HasSpan for UnknownExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_expression(node: &ExpressionNode, sources: &SourceCollection) -> Expression {
    match node {
        ExpressionNode::Literal(x) => transform_literal_expression(x, sources),
        ExpressionNode::Block(x) => transform_block_expression(x, sources),
        ExpressionNode::If(x) => transform_if_expression(x, sources),
        ExpressionNode::Add(x) => transform_plus_expression(x, sources),
        ExpressionNode::Mul(x) => transform_mul_expression(x, sources),
        ExpressionNode::Compare(x) => transform_compare_expression(x, sources),
        ExpressionNode::Access(x) => transform_access_expression(x, sources),
    }
}

pub fn transform_access_expression(
    node: &AccessExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::access(node.span(), sources.get_identifier(node.identifier.span()))
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

pub fn transform_block_expression(
    node: &BlockExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::block(
        node.span(),
        vec![],
        node.value
            .as_ref()
            .map(|n| transform_expression(n, sources)),
    )
}

pub fn transform_if_expression(node: &IfExpressionNode, sources: &SourceCollection) -> Expression {
    Expression::if_(
        node.span(),
        node.condition
            .as_ref()
            .map(|x| transform_expression(x.as_ref(), sources))
            .unwrap_or_else(|| Expression::unknown()),
        node.then
            .as_ref()
            .map(|x| transform_expression(x.as_ref(), sources))
            .unwrap_or_else(|| Expression::unknown()),
        node.else_
            .as_ref()
            .map(|x| transform_expression(x.as_ref(), sources)),
    )
}

pub fn transform_compare_expression(
    node: &CompareExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::binary(
        node.span(),
        match node.operator.token_type {
            TokenType::DoubleEquals => BinaryOperator::Equals,
            TokenType::NotEquals => BinaryOperator::NotEquals,
            TokenType::LessThan => BinaryOperator::LessThan,
            TokenType::LessOrEquals => BinaryOperator::LessThanOrEquals,
            TokenType::GreaterThan => BinaryOperator::GreaterThan,
            TokenType::GreaterOrEquals => BinaryOperator::GreaterThanOrEquals,
            _ => unreachable!(),
        },
        match &node.left {
            Some(left) => transform_expression(left.as_ref(), sources),
            None => Expression::unknown(),
        },
        match &node.right {
            Some(right) => transform_expression(right.as_ref(), sources),
            None => Expression::unknown(),
        },
    )
}

pub fn transform_plus_expression(
    node: &AddExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    fn map_op(token_type: TokenType) -> BinaryOperator {
        match token_type {
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Minus => BinaryOperator::Minus,
            _ => unreachable!(),
        }
    }

    fn map_expr(node: Option<&ExpressionNode>, sources: &SourceCollection) -> Expression {
        match node {
            Some(expr) => transform_expression(expr, sources),
            None => Expression::unknown(),
        }
    }

    let mut result = map_expr(Some(&node.left), sources);

    for follow in &node.follows {
        result = Expression::binary(
            result.span() + follow.operator.span() + follow.operand.span(),
            map_op(follow.operator.token_type),
            result,
            map_expr(follow.operand.as_ref(), sources),
        );
    }

    result
}

pub fn transform_mul_expression(
    node: &MulExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    todo!()
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{
        transform_expression, BinaryExpression, BinaryOperator, BlockExpression, Expression,
        ExpressionKind, IfExpression, LiteralExpression, LiteralValue,
    };
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::parser::compare_expression_node::CompareExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::if_expression_node::IfExpressionNode;
    use crate::parser::literal_expression_node::{
        BooleanLiteralNode, IntegerLiteralNode, LiteralExpressionNode,
    };
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::*;
    use crate::tokenizer::{Token, TokenType};
    use parameterized::parameterized;
    use ustr::ustr;

    #[test]
    fn transform_access_expression() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foobar");

        let expr_node =
            ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span)));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(expr, Expression::access(span, ustr("foobar")));
    }

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

    #[test]
    fn transform_empty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ }");

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(span, vec![], None));

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: span,
                    statements: vec![],
                    value: None,
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_nonempty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ 123 }");
        let num_span = span.sub(2..5);

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            span,
            vec![],
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    num_span,
                    test_token!(DecInteger:num_span),
                    false,
                )),
            ))),
        ));

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: span,
                    statements: vec![],
                    value: Some(Box::new(Expression::int_literal(num_span, 123))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_nested_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ { } }");
        let inner_span: Span = span.sub(2..5);

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(
            span,
            vec![],
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                inner_span,
                vec![],
                None,
            )))),
        ));

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Block(BlockExpression {
                    span_: span,
                    statements: vec![],
                    value: Some(Box::new(Expression::block(inner_span, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_single_add() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 + 2");

        let add_node = ExpressionNode::Add(AddExpressionNode::new(
            span,
            Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(0, test_token!(DecInteger:0), false),
            ))),
            vec![AddExpressionNodeFollow {
                operator: test_token!(Plus:2),
                operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                    IntegerLiteralNode::new(4, test_token!(DecInteger:4), false),
                ))),
            }],
        ));

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span,
                    op: BinaryOperator::Plus,
                    left: Box::new(Expression::int_literal(0, 1)),
                    right: Box::new(Expression::int_literal(4, 2)),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_multi_add() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 + 2 - 3");

        let span_1 = span.sub(0..1);
        let span_plus = span.sub(2..3);
        let span_2 = span.sub(4..5);
        let span_minus = span.sub(6..7);
        let span_3 = span.sub(8..9);

        let add_node = ExpressionNode::Add(AddExpressionNode::new(
            span,
            Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(span_1, test_token!(DecInteger:span_1), false),
            ))),
            vec![
                AddExpressionNodeFollow {
                    operator: test_token!(Plus:span_plus),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_2, test_token!(DecInteger:span_2), false),
                    ))),
                },
                AddExpressionNodeFollow {
                    operator: test_token!(Minus:span_minus),
                    operand: Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                        IntegerLiteralNode::new(span_3, test_token!(DecInteger:span_3), false),
                    ))),
                },
            ],
        ));

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span_1 + span_3,
                    op: BinaryOperator::Minus,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Binary(BinaryExpression {
                            span_: span_1 + span_2,
                            op: BinaryOperator::Plus,
                            left: Box::new(Expression::int_literal(span_1, 1)),
                            right: Box::new(Expression::int_literal(span_2, 2)),
                        }),
                        type_ref: None,
                    }),
                    right: Box::new(Expression::int_literal(span_3, 3)),
                }),
                type_ref: None,
            }
        );
    }

    #[parameterized(values = {
        (TokenType::DoubleEquals, BinaryOperator::Equals),
        (TokenType::NotEquals, BinaryOperator::NotEquals),
        (TokenType::LessOrEquals, BinaryOperator::LessThanOrEquals),
        (TokenType::GreaterOrEquals, BinaryOperator::GreaterThanOrEquals),
        (TokenType::LessThan, BinaryOperator::LessThan),
        (TokenType::GreaterThan, BinaryOperator::GreaterThan),
    })]
    fn transform_compare(values: (TokenType, BinaryOperator)) {
        let (op_token, op_expected) = values;
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 ·· 2");
        let span_1 = span.sub(0..1);
        let span_op = span.sub(2..4);
        let span_2 = span.sub(5..6);

        let compare_node = ExpressionNode::Compare(CompareExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    span_1,
                    test_token!(DecInteger:span_1),
                    false,
                )),
            ))),
            Token::new(span_op, op_token),
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    span_2,
                    test_token!(DecInteger:span_2),
                    false,
                )),
            ))),
        ));

        // act
        let expr = transform_expression(&compare_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    span_: span,
                    op: op_expected,
                    left: Box::new(Expression::int_literal(span_1, 1)),
                    right: Box::new(Expression::int_literal(span_2, 2)),
                }),
                type_ref: None,
            }
        );
    }

    #[test]
    fn transform_if() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("if 1 {} else {}");

        let if_node = ExpressionNode::If(IfExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    0,
                    test_token!(DecInteger:3),
                    false,
                )),
            ))),
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                5..7,
                vec![],
                None,
            )))),
            Some(Box::new(ExpressionNode::Block(BlockExpressionNode::new(
                14..16,
                vec![],
                None,
            )))),
        ));

        // act
        let expr = transform_expression(&if_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression {
                kind: ExpressionKind::If(IfExpression {
                    span_: span,
                    condition: Box::new(Expression::int_literal(0, 1)),
                    then: Box::new(Expression::block(5..7, vec![], None)),
                    else_: Some(Box::new(Expression::block(14..16, vec![], None))),
                }),
                type_ref: None,
            }
        );
    }
}
