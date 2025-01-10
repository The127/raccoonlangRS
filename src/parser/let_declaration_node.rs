use crate::awesome_iterator::AwesomeIterator;
use crate::errors::{ErrorKind, Errors};
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::parser::recover_until;
use crate::source_map::{HasSpan, Span};
use crate::{consume_token, expect_token, token_starter};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetDeclarationNode {
    span_: Span,
    pub binding: Option<Token>,
    pub value: Option<ExpressionNode>,
}

impl HasSpan for LetDeclarationNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_let_declaration<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<LetDeclarationNode> {
    token_starter!(let_starter, Let);
    token_starter!(binding_starter, Identifier);
    token_starter!(value_starter, Equals);

    let mut recover_errors = Errors::new();
    let mut mark = iter.mark();

    if !recover_until(&mut mark, &mut recover_errors, [let_starter], []) {
        mark.reset();
        return None;
    }
    mark.discard();

    errors.merge(recover_errors);

    let let_token = expect_token!(iter, Let);

    recover_until(iter, errors, [binding_starter], []);

    let binding = consume_token!(iter, Identifier);

    let mut recover_errors = Errors::new();

    let mut result = LetDeclarationNode {
        span_: let_token.span() + binding.span(),
        binding,
        value: None,
    };

    let mut mark = iter.mark();

    if !recover_until(&mut mark, &mut recover_errors, [value_starter], []) {
        mark.reset();
        return Some(result);
    }
    mark.discard();

    errors.merge(recover_errors);

    if let Some(eq_token) = consume_token!(iter, Equals) {
        let value = parse_expression(iter, errors, true);

        if value.is_none() {
            errors.add(ErrorKind::MissingLetDeclarationValue, eq_token.span().end());
        }

        result.span_ += eq_token.span() + value.span();
        result.value = value;
    }

    Some(result)
}

#[cfg(test)]
mod test {
    use crate::awesome_iterator::make_awesome;
    use crate::errors::{ErrorKind, Errors};
    use crate::{test_token, test_tokentree};
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::tokenizer::TokenType::*;
    use super::*;

    #[test]
    fn empty() {
        // arrange
        let tokens = test_tokentree!();
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_let_declaration(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn unknown_token() {
        // arrange
        let tokens = test_tokentree!(Unknown);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_let_declaration(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn simple_let_decl() {
        // arrange
        let tokens = test_tokentree!(Let:7..10, Identifier:12..20);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_let_declaration(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(LetDeclarationNode {
            span_: (7..20).into(),
            binding: Some(test_token!(Identifier:12..20)),
            value: None,
        }));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }

    #[test]
    fn let_with_value() {
        // arrange
        let tokens = test_tokentree!(Let:7..10, Identifier:12..20, Equals:22, DecInteger:24..26);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_let_declaration(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(LetDeclarationNode {
            span_: (7..26).into(),
            binding: Some(test_token!(Identifier:12..20)),
            value: Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
                IntegerLiteralNode::new(24..26, test_token!(DecInteger:24..26), false))
            )),
        }));
        assert!(errors.get_errors().is_empty());
        assert!(remaining.is_empty());
    }
    
    #[test]
    fn simple_let_with_leftover() {
        // arrange
        let tokens = test_tokentree!(Let:7..10, Identifier:12..20, Unknown:22);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_let_declaration(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(LetDeclarationNode {
            span_: (7..20).into(),
            binding: Some(test_token!(Identifier:12..20)),
            value: None,
        }));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown:22).iter().collect::<Vec<_>>());
    }

    #[test]
    fn missing_value_after_equals() {
        // arrange
        let tokens = test_tokentree!(Let:7..10, Identifier:12..20, Equals:22);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_let_declaration(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(LetDeclarationNode {
            span_: (7..23).into(),
            binding: Some(test_token!(Identifier:12..20)),
            value: None,
        }));
        assert!(errors.has_error_at(23, ErrorKind::MissingLetDeclarationValue));
        assert_eq!(errors.get_errors().len(), 1);
        assert!(remaining.is_empty());
    }
}