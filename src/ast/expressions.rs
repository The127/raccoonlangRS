use crate::ast::expressions::CompareExpressionOperator::{
    OpEquals, OpGreaterOrEquals, OpGreaterThan, OpLessOrEquals, OpLessThan, OpNotEquals,
};
use crate::ast::statement::Statement;
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Access(AccessExpression),
    Block(BlockExpression),
    Add(AddExpression),
    If(IfExpression),
    Compare(CompareExpression),
    Unknown(UnknownExpression),
}

impl Expression {
    pub fn unknown() -> Self {
        Expression::Unknown(UnknownExpression {
            span_: Span::empty(),
        })
    }

    pub fn block<S: Into<Span>>(
        span: S,
        statements: Vec<Statement>,
        value: Option<Expression>,
    ) -> Self {
        Expression::Block(BlockExpression {
            span_: span.into(),
            statements,
            value: value.map(Box::new),
        })
    }

    pub fn int_literal<S: Into<Span>>(span: S, value: i32) -> Self {
        Expression::Literal(LiteralExpression {
            span_: span.into(),
            value: LiteralValue::Integer(value),
        })
    }

    pub fn access<S: Into<Span>>(span: S, name: Ustr) -> Self {
        Expression::Access(AccessExpression {
            span_: span.into(),
            name,
        })
    }

    pub fn add<S: Into<Span>>(
        span: S,
        left: Expression,
        follows: Vec<(AddExpressionOperator, Expression)>,
    ) -> Self {
        Expression::Add(AddExpression {
            span_: span.into(),
            left: Box::new(left),
            follows: follows.into_iter().map(|(op, expr)| AddExpressionFollow {
                operator: op,
                operand: Some(Box::new(expr)),
            }).collect()
        })
    }

    pub fn if_<S: Into<Span>>(
        span: S,
        condition: Expression,
        then: Expression,
        else_: Option<Expression>,
    ) -> Self {
        Expression::If(IfExpression {
            span_: span.into(),
            condition: Some(Box::new(condition)),
            then: Some(Box::new(then)),
            else_: else_.map(Box::new),
        })
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AddExpression {
    span_: Span,
    pub left: Box<Expression>,
    pub follows: Vec<AddExpressionFollow>,
}

impl HasSpan for AddExpression {
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
    pub condition: Option<Box<Expression>>,
    pub then: Option<Box<Expression>>,
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
        ExpressionNode::Compare(x) => transform_compare_expression(x, sources),
        ExpressionNode::Access(x) => transform_access_expression(x, sources),
    }
}

pub fn transform_access_expression(
    node: &AccessExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::Access(AccessExpression {
        span_: node.span(),
        name: sources.get_identifier(node.identifier.span()),
    })
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
    Expression::If(IfExpression {
        span_: node.span(),
        condition: node
            .condition
            .as_ref()
            .map(|x| Box::new(transform_expression(x.as_ref(), sources))),
        then: node
            .then
            .as_ref()
            .map(|x| Box::new(transform_expression(x.as_ref(), sources))),
        else_: node
            .else_
            .as_ref()
            .map(|x| Box::new(transform_expression(x.as_ref(), sources))),
    })
}

pub fn transform_compare_expression(
    node: &CompareExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::Compare(CompareExpression {
        span_: node.span(),
        left: match &node.left {
            Some(left) => Some(Box::new(transform_expression(left.as_ref(), sources))),
            None => None,
        },
        operator: match node.operator.token_type {
            TokenType::DoubleEquals => OpEquals,
            TokenType::NotEquals => OpNotEquals,
            TokenType::LessThan => OpLessThan,
            TokenType::LessOrEquals => OpLessOrEquals,
            TokenType::GreaterThan => OpGreaterThan,
            TokenType::GreaterOrEquals => OpGreaterOrEquals,
            _ => unreachable!(),
        },
        right: match &node.right {
            Some(right) => Some(Box::new(transform_expression(right.as_ref(), sources))),
            None => None,
        },
    })
}

pub fn transform_plus_expression(
    node: &AddExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    let mut result = AddExpression {
        span_: node.span(),
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
    use crate::ast::expressions::{
        transform_expression, AddExpression, AddExpressionFollow,
        CompareExpression, CompareExpressionOperator, Expression, IfExpression,
    };
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};
    use crate::parser::block_expression_node::BlockExpressionNode;
    use crate::parser::compare_expression_node::CompareExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::if_expression_node::IfExpressionNode;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::*;
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

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(BinInteger:span), false),
        ));

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

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(OctInteger:span), false),
        ));

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

        let expr_node = ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(span, test_token!(HexInteger:span), false),
        ));

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

        let block_node = ExpressionNode::Block(BlockExpressionNode::new(span, vec![], None));

        // act
        let expr = transform_expression(&block_node, &sources);

        // assert
        assert_eq!(expr, Expression::block(span, vec![], None));
    }

    #[test]
    fn transform_nonempty_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ 123 }");
        let num_span: Span = ((span.start() + 2)..(span.end() - 2)).into();

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
            Expression::block(span, vec![], Some(Expression::int_literal(num_span, 123)))
        );
    }

    #[test]
    fn transform_nested_block() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("{ { } }");
        let inner_span: Span = ((span.start() + 2)..(span.end() - 2)).into();

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
            Expression::block(
                span,
                vec![],
                Some(Expression::block(inner_span, vec![], None))
            )
        );
    }

    #[test]
    fn transform_add() {
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
                operand: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        4,
                        test_token!(DecInteger:4),
                        false,
                    )),
                ))),
            }],
        ));

        // act
        let expr = transform_expression(&add_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::Add(AddExpression {
                span_: span,
                left: Box::new(Expression::int_literal(0, 1)),
                follows: vec![AddExpressionFollow {
                    operator: OpPlus,
                    operand: Some(Box::new(Expression::int_literal(4, 2))),
                }],
            })
        );
    }

    #[test]
    fn transform_compare() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("1 == 2");

        let compare_node = ExpressionNode::Compare(CompareExpressionNode::new(
            span,
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    0,
                    test_token!(DecInteger:0),
                    false,
                )),
            ))),
            test_token!(DoubleEquals:2..4),
            Some(Box::new(ExpressionNode::Literal(
                LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                    5,
                    test_token!(DecInteger:5),
                    false,
                )),
            ))),
        ));

        // act
        let expr = transform_expression(&compare_node, &sources);

        // assert
        assert_eq!(
            expr,
            Expression::Compare(CompareExpression {
                span_: span,
                left: Some(Box::new(Expression::int_literal(0, 1))),
                operator: CompareExpressionOperator::OpEquals,
                right: Some(Box::new(Expression::int_literal(5, 2))),
            })
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
            Expression::If(IfExpression {
                span_: span,
                condition: Some(Box::new(Expression::int_literal(0, 1))),
                then: Some(Box::new(Expression::block(5..7, vec![], None))),
                else_: Some(Box::new(Expression::block(14..16, vec![], None))),
            })
        );
    }
}
