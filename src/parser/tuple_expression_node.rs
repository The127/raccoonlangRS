use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::{add_error, consume_token};
use crate::errors::Errors;
use crate::parser::consume_group;
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TupleExpressionNode {
    span_: Span,
    pub values: Vec<ExpressionNode>,
}

impl TupleExpressionNode {
    pub fn new<S: Into<Span>>(span: S, values: Vec<ExpressionNode>) -> Self {
        Self {
            span_: span.into(),
            values,
        }
    }
}

impl HasSpan for TupleExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_tuple_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    let mut mark = iter.mark().auto_reset();

    if let Some(group) = consume_group(&mut mark, TokenType::OpenParen) {
        let mut iter = make_awesome(group.children.iter());
        let mut exprs = vec![];

        let mut is_tuple = false;
        let mut missing_comma_location = None;

        if let Some(extra_comma) = consume_token!(&mut iter, Comma) {
            is_tuple = true;
            add_error!(errors, extra_comma.span(), UnexpectedToken(TokenType::Comma));
        }

        loop {
            if let Some(expr) = parse_expression(&mut iter, errors, true) {
                if let Some(loc) = missing_comma_location {
                    is_tuple = true;
                    add_error!(errors, loc, MissingComma);
                }

                if consume_token!(&mut iter, Comma).is_some() {
                    is_tuple = true;
                } else {
                    missing_comma_location = Some(expr.span().end());
                }

                while let Some(extra_comma) = consume_token!(&mut iter, Comma) {
                    add_error!(errors, extra_comma.span(), UnexpectedToken(TokenType::Comma));
                }

                exprs.push(expr);
            } else {
                break;
            }
        }

        if is_tuple {
            mark.discard();
            return Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: group.span(),
                values: exprs,
            }));
        }
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(
            remaining,
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn empty_parentheses() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(());
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!(()).iter().collect::<Vec<_>>());
    }

    #[test]
    fn single_expr_in_parens() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!((Identifier));
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(
            remaining,
            test_tokentree!((Identifier)).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_expr_with_comma() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!( (:1, Identifier:2..5, Comma:6 ):7 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span(1, 8),
                values: vec![ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier:2..5)
                ))],
            }))
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn multiple_exprs() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((Identifier, Comma, Identifier, Comma, Identifier,));
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span::empty(),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier))),
                ],
            }))
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn multiple_exprs_trailing_comma() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((Identifier, Comma, Identifier, Comma, Identifier, Comma, ));
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span::empty(),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier))),
                ],
            }))
        );
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn multiple_commas_in_a_row() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:1, Identifier:2..5, Comma:6, Comma:7, Comma:8, Identifier:9..13, ):14);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span(1, 15),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:2..5))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:9..13))),
                ],
            }))
        );
        assert_eq!(errors.get_errors().len(), 2);
        assert!(errors.has_error_at(7, ErrorKind::UnexpectedToken(Comma)));
        assert!(errors.has_error_at(8, ErrorKind::UnexpectedToken(Comma)));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn starting_comma() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:1, Comma:2, Identifier:3..5 ):6);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span(1, 7),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:3..5))),
                ],
            }))
        );

        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(2, ErrorKind::UnexpectedToken(Comma)));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn missing_first_comma() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:1, Identifier:2..5, Identifier:7..10 ):11);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span(1, 12),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:2..5))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:7..10))),
                ],
            }))
        );

        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(5, ErrorKind::MissingComma));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn missing_later_comma() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:1, Identifier:2..5, Comma:6, Identifier:7..10, Identifier:12..15):16);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span(1, 17),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:2..5))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:7..10))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:12..15))),
                ],
            }))
        );

        assert_eq!(errors.get_errors().len(), 1);
        assert!(errors.has_error_at(10, ErrorKind::MissingComma));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn missing_multiple_commas() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!((:1, Identifier:2..5, Identifier:7..10, Identifier:12..15):16);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_tuple_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Tuple(TupleExpressionNode {
                span_: Span(1, 17),
                values: vec![
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:2..5))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:7..10))),
                    ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:12..15))),
                ],
            }))
        );

        assert_eq!(errors.get_errors().len(), 2);
        assert!(errors.has_error_at(5, ErrorKind::MissingComma));
        assert!(errors.has_error_at(10, ErrorKind::MissingComma));
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
