use crate::errors::{ErrorKind, Errors};
use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::parser::consume_group;
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType::OpenCurly;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpressionNode {
    span_: Span,
    pub value: Option<Box<ExpressionNode>>,
}

impl BlockExpressionNode {
    pub fn new<S: Into<Span>>(span: S, value: Option<Box<ExpressionNode>>) -> Self {
        Self {
            span_: span.into(),
            value,
        }
    }
}

impl HasSpan for BlockExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}


// #[derive(Debug, Eq, PartialEq, Clone)]
// pub struct StatementNode {
//     span_: Span,
//     pub kind: StatementKind,
// }
//
// impl HasSpan for StatementNode {
//     fn span(&self) -> Span {
//         self.span_
//     }
// }
//
// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum StatementKind {
//     Expression(ExpressionNode),
//     Declaration(DeclarationNode),
// }
//
// impl HasSpan for StatementKind {
//     fn span(&self) -> Span {
//         match self {
//             StatementKind::Expression(x) => x.span(),
//             StatementKind::Declaration(x) => x.span(),
//         }
//     }
// }




pub fn parse_block_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    let group = consume_group(iter, OpenCurly)?;
    let mut iter = make_awesome(group.children.iter());
    let value = parse_expression(&mut iter, errors);

    match iter.next() {
        Some(TokenTree::Token(token))=> errors.add(ErrorKind::UnexpectedToken(token.token_type), token.span()),
        Some(TokenTree::Group(group)) => errors.add(ErrorKind::UnexpectedToken(group.open.token_type), group.open.span()),
        _ => ()
    };

    Some(ExpressionNode::Block(BlockExpressionNode {
        span_: group.span(),
        value: value.map(Box::new),
    }))
}

#[cfg(test)]
mod test {
    use crate::errors::{ErrorKind, Errors};
    use crate::awesome_iterator::make_awesome;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_expression_empty_input(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_unknown_input(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_empty_block(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Block(BlockExpressionNode {
            span_: (4..121).into(),
            value: None,
        })));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_just_value(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Block(BlockExpressionNode {
            span_: (4..121).into(),
            value: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(60..70, test_token!(DecInteger:60..70), false))
            ))),
        })));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_expression_unexpected_token_after_value(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, Unknown:75..80 }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Block(BlockExpressionNode {
            span_: (4..121).into(),
            value: Some(Box::new(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(60..70, test_token!(DecInteger:60..70), false)
            )))),
        })));
        assert!(errors.has_error_at(75..80, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}