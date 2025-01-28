use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::Errors;
use crate::parser::file_node::toplevel_starter;
use crate::parser::type_node::{parse_type, type_starter, TypeNode};
use crate::parser::{consume_group, recover_until, Spanned, Visibility};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::OpenParen;
use crate::treeizer::TokenTree;
use crate::{add_error, consume_token, expect_token, token_starter};

#[derive(Debug, Default, Eq, PartialEq)]
pub struct StructNode {
    span_: Span,
    pub visibility: Visibility,
    pub name: Option<Token>,
    pub members: Vec<StructMemberNode>,
}

impl HasSpan for StructNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl StructNode {
    pub fn new<S: Into<Span>>(
        span: S,
        visibility: Visibility,
        name: Option<Token>,
        members: Vec<StructMemberNode>,
    ) -> Self {
        Self {
            span_: span.into(),
            visibility,
            name,
            members,
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct StructMemberNode {
    span_: Span,
    pub name: Token,
    pub type_: Option<TypeNode>,
}

impl HasSpan for StructMemberNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl StructMemberNode {
    pub fn new<S: Into<Span>>(
        span: S,
        name: Token,
        type_: Option<TypeNode>,
    ) -> Self {
        Self {
            span_: span.into(),
            name,
            type_,
        }
    }
}

pub fn parse_struct<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<StructNode> {
    let mut result = StructNode::default();

    let mut mark = iter.mark();

    token_starter!(struct_starter, Struct);

    if let Some(pub_token) = consume_token!(&mut mark, Pub) {
        result.span_ = pub_token.span();
        result.visibility = Visibility::Public(pub_token);

        let mut recover_errors = Errors::new();
        if !recover_until(
            &mut mark,
            &mut recover_errors,
            [struct_starter],
            [toplevel_starter],
        ) {
            mark.reset();
            return None;
        }

        errors.merge(recover_errors);
    }

    if let Some(struct_token) = consume_token!(&mut mark, Struct) {
        result.span_ += struct_token.span();
        mark.discard();
    } else {
        mark.reset();
        return None;
    };

    if let Some(name) = consume_token!(iter, Identifier) {
        result.span_ += name.span();
        result.name = Some(name);
    } else {
        add_error!(errors, result.span_.end(), MissingDeclarationName);
    }

    if let Some(body) = parse_struct_body(iter, errors) {
        result.span_ += body.span_;
        result.members = body.value;
    } else {
        add_error!(errors, result.span_.end(), MissingStructBody);
    }

    Some(result)
}


pub fn parse_struct_body<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<Spanned<Vec<StructMemberNode>>> {
    let mut iter = iter.mark();
    let mut result = vec![];

    let (mut iter, group_span) = match consume_group(&mut iter, OpenParen) {
        Some(group) => (make_awesome(group.children.iter()), group.span()),
        _ => {
            iter.reset();
            return None;
        }
    };

    token_starter!(identifier, Identifier);
    token_starter!(comma, Comma);
    token_starter!(colon, Colon);
    while recover_until(&mut iter, errors, [identifier], []) {
        let name = expect_token!(&mut iter, Identifier);

        result.push(StructMemberNode {
            span_: name.span(),
            name: name,
            type_: None,
        });
        let param = result.last_mut().expect("literally just pushed");

        if !recover_until(&mut iter, errors, [colon, type_starter, comma], []) {
            break;
        }

        if let Some(colon_token) = consume_token!(&mut iter, Colon) {
            param.span_ += colon_token.span();
        } else {
            add_error!(errors, param.span_.end(), MissingColon);
        }

        if !recover_until(&mut iter, errors, [type_starter, comma], []) {
            break;
        }

        if let Some(type_) = parse_type(&mut iter, errors) {
            param.span_ += type_.span();
            param.type_ = Some(type_);
        } else {
            add_error!(errors, param.span_.end(), MissingStructMemberType);
        }

        if !recover_until(&mut iter, errors, [comma, identifier], []) {
            break;
        }

        if consume_token!(&mut iter, Comma).is_none() {
            add_error!(errors, param.span_.end(), MissingComma);
        }
    }

    Some(Spanned {
        span_: group_span,
        value: result,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::NamedTypeNode;
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokens, test_tokentree};

    #[test]
    fn empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn empty_struct() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Struct:1..7, Identifier:8..11, (:12, ):13 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(StructNode {
                span_: Span(1, 14),
                visibility: Visibility::Module,
                name: Some(test_token!(Identifier:8..11)),
                members: vec![],
            })
        );
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn pub_struct() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Pub:3..6, Struct:10..16, Identifier:18..21, (:22, ):23 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(StructNode {
                span_: Span(3, 24),
                visibility: Visibility::Public(test_token!(Pub:3..6)),
                name: Some(test_token!(Identifier:18..21)),
                members: vec![],
            })
        );
        errors.assert_empty();
        assert!(remaining.is_empty());
    }

    #[test]
    fn missing_name() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:3..6, Struct:10..16, (:22, ):23 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(StructNode {
                span_: Span(3, 24),
                visibility: Visibility::Public(test_token!(Pub:3..6)),
                name: None,
                members: vec![],
            })
        );
        assert!(remaining.is_empty());
        assert!(errors.has_error_at(16, ErrorKind::MissingDeclarationName));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn missing_body() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!(Pub:3..6, Struct:10..16, Identifier:18..20, Fn:22..24);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(StructNode {
                span_: Span(3, 20),
                visibility: Visibility::Public(test_token!(Pub:3..6)),
                name: Some(test_token!(Identifier:18..20)),
                members: vec![],
            })
        );
        assert_eq!(
            remaining,
            test_tokentree!(Fn:22..24).iter().collect::<Vec<_>>()
        );
        assert!(errors.has_error_at(20, ErrorKind::MissingStructBody));
        assert_eq!(errors.get_errors().len(), 1);
    }

    #[test]
    fn member() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Pub:1, Struct:2, Identifier:3, (:4, Identifier:6..8, Colon:9, Identifier:11..15, ):17 );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(StructNode {
                span_: Span(1, 18),
                visibility: Visibility::Public(test_token!(Pub:1)),
                name: Some(test_token!(Identifier:3)),
                members: vec![StructMemberNode {
                    span_: Span(6, 15),
                    name: test_token!(Identifier:6..8),
                    type_: Some(TypeNode::Named(NamedTypeNode::new(
                        11..15,
                        PathNode::new(11..15, test_tokens!(Identifier:11..15), false)
                    ))),
                },],
            })
        );
    }

    #[test]
    fn multiple_members() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(
            Pub,
            Struct,
            Identifier,
            (
                Identifier, Colon, Identifier, Comma, Identifier, Colon, Identifier, Comma,
                Identifier, Colon, Identifier, Comma,
            )
        );
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_struct(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(StructNode {
                span_: Span::empty(),
                visibility: Visibility::Public(test_token!(Pub)),
                name: Some(test_token!(Identifier)),
                members: vec![
                    StructMemberNode {
                        span_: Span::empty(),
                        name: test_token!(Identifier),
                        type_: Some(TypeNode::Named(NamedTypeNode::new(
                            Span::empty(),
                            PathNode::new(Span::empty(), test_tokens!(Identifier), false)
                        ))),
                    },
                    StructMemberNode {
                        span_: Span::empty(),
                        name: test_token!(Identifier),
                        type_: Some(TypeNode::Named(NamedTypeNode::new(
                            Span::empty(),
                            PathNode::new(Span::empty(), test_tokens!(Identifier), false)
                        ))),
                    },
                    StructMemberNode {
                        span_: Span::empty(),
                        name: test_token!(Identifier),
                        type_: Some(TypeNode::Named(NamedTypeNode::new(
                            Span::empty(),
                            PathNode::new(Span::empty(), test_tokens!(Identifier), false)
                        ))),
                    },
                ],
            })
        );
    }
}
