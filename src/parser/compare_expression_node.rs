use crate::ast::expressions::CompareExpression;
use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::{ErrorKind, Errors};
use crate::parser::add_expression_node::parse_add_expression;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::recover_until;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType::*;
use crate::tokenizer::{Token, TokenType};
use crate::treeizer::TokenTree;
use crate::{consume_token, token_starter};

#[derive(Debug, Eq, PartialEq)]
pub struct CompareExpressionNode {
    span_: Span,
    pub left: Option<Box<ExpressionNode>>,
    pub operator: Token,
    pub right: Option<Box<ExpressionNode>>,
}

impl CompareExpressionNode {
    pub fn new<S: Into<Span>>(
        span: S,
        left: Option<Box<ExpressionNode>>,
        operator: Token,
        right: Option<Box<ExpressionNode>>,
    ) -> Self {
        CompareExpressionNode {
            span_: span.into(),
            left,
            operator,
            right,
        }
    }
}

impl HasSpan for CompareExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_compare_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    token_starter!(
        op_starter,
        DoubleEquals | NotEquals | LessThan | LessOrEquals | GreaterThan | GreaterOrEquals
    );

    let left = {
        let mut sub_iter = iter.until(op_starter);
        let add = parse_add_expression(&mut sub_iter, errors);
        recover_until(&mut sub_iter, errors, [], []);
        add
    };

    if let Some(op) = consume_token!(
        iter,
        DoubleEquals | NotEquals | LessThan | LessOrEquals | GreaterThan | GreaterOrEquals
    ) {
        let right = parse_compare_expression(iter, errors);

        if left.is_none() {
            errors.add(ErrorKind::MissingOperand, op.span().start());
        }

        match right {
            None => errors.add(ErrorKind::MissingOperand, op.span().end()),
            Some(ExpressionNode::Compare(CompareExpressionNode {
                operator: right_op, ..
            })) => errors.add(
                ErrorKind::AmbiguousComparisonExpression(op.span()),
                right_op.span(),
            ),
            _ => (),
        }

        Some(ExpressionNode::Compare(CompareExpressionNode {
            span_: left.span() + op.span() + right.span(),
            left: left.map(Box::new),
            operator: op,
            right: right.map(Box::new),
        }))
    } else {
        return left;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::ErrorKind;
    use crate::errors::ErrorKind::UnexpectedToken;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::{test_token, test_tokentree};
    use parameterized::parameterized;

    #[test]
    fn parse_compare_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(Span::empty(), UnexpectedToken(Unknown)));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_just_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false),
            )))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[parameterized(op = {
        DoubleEquals, NotEquals, LessThan, LessOrEquals, GreaterThan, GreaterOrEquals
    })]
    fn parse_compare_expression_equals(op: TokenType) {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, op:3..5, DecInteger:7..12);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: (1..12).into(),
                left: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        1..2,
                        test_token!(DecInteger:1..2),
                        false
                    ),)
                ))),
                operator: test_token!(op:3..5),
                right: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        7..12,
                        test_token!(DecInteger:7..12),
                        false
                    ))
                ))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_multiple() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, DoubleEquals:3..5, DecInteger:7..12, DoubleEquals:15..17, DecInteger:19..25);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: (1..25).into(),
                left: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        1..2,
                        test_token!(DecInteger:1..2),
                        false
                    ),)
                ))),
                operator: test_token!(DoubleEquals:3..5),
                right: Some(Box::new(ExpressionNode::Compare(CompareExpressionNode {
                    span_: (7..25).into(),
                    left: Some(Box::new(ExpressionNode::Literal(
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                            7..12,
                            test_token!(DecInteger:7..12),
                            false
                        ),)
                    ))),
                    operator: test_token!(DoubleEquals:15..17),
                    right: Some(Box::new(ExpressionNode::Literal(
                        LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                            19..25,
                            test_token!(DecInteger:19..25),
                            false
                        ))
                    ))),
                }))),
            }))
        );
        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(
            15..17,
            ErrorKind::AmbiguousComparisonExpression((3..5).into())
        ));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_missing_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DoubleEquals:3..5, DecInteger:7..12);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: (3..12).into(),
                left: None,
                operator: test_token!(DoubleEquals:3..5),
                right: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        7..12,
                        test_token!(DecInteger:7..12),
                        false
                    ))
                ))),
            }))
        );
        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(3, ErrorKind::MissingOperand));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_missing_right() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2, DoubleEquals:3..5);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: (1..5).into(),
                left: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Integer(IntegerLiteralNode::new(
                        1..2,
                        test_token!(DecInteger:1..2),
                        false
                    ),)
                ))),
                operator: test_token!(DoubleEquals:3..5),
                right: None,
            }))
        );
        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(5, ErrorKind::MissingOperand));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_compare_expression_missing_both() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DoubleEquals:3..5);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_compare_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Compare(CompareExpressionNode {
                span_: (3..5).into(),
                left: None,
                operator: test_token!(DoubleEquals:3..5),
                right: None,
            }))
        );
        assert_eq!(errors.get_errors().len(), 2);
        assert!(errors.has_error_at(3, ErrorKind::MissingOperand));
        assert!(errors.has_error_at(5, ErrorKind::MissingOperand));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
