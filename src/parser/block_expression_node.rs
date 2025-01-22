use crate::awesome_iterator::{make_awesome, AwesomeIterator};
use crate::errors::{ErrorKind, Errors};
use crate::parser::expression_node::{parse_expression, ExpressionNode};
use crate::parser::let_declaration_node::{parse_let_declaration, LetDeclarationNode};
use crate::parser::{consume_group, recover_until};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType::OpenCurly;
use crate::treeizer::TokenTree;
use crate::{add_error, consume_token, token_starter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpressionNode {
    span_: Span,
    pub statements: Vec<StatementNode>,
    pub value: Option<Box<ExpressionNode>>,
}

impl BlockExpressionNode {
    pub fn new<S: Into<Span>>(
        span: S,
        statements: Vec<StatementNode>,
        value: Option<Box<ExpressionNode>>,
    ) -> Self {
        Self {
            span_: span.into(),
            statements,
            value,
        }
    }
}

impl HasSpan for BlockExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StatementNode {
    span_: Span,
    pub kind: StatementKind,
}

impl HasSpan for StatementNode {
    fn span(&self) -> Span {
        self.span_
    }
}

impl StatementNode {
    pub fn decl<S: Into<Span>>(span: S, decl: LetDeclarationNode) -> Self {
        Self {
            span_: span.into(),
            kind: StatementKind::Declaration(decl),
        }
    }

    pub fn expr<S: Into<Span>>(span: S, expr: ExpressionNode) -> Self {
        Self {
            span_: span.into(),
            kind: StatementKind::Expression(expr),
        }
    }

    pub fn empty<S: Into<Span>>(span: S) -> Self {
        Self {
            span_: span.into(),
            kind: StatementKind::Empty,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StatementKind {
    Empty,
    Expression(ExpressionNode),
    Declaration(LetDeclarationNode),
}

impl HasSpan for StatementKind {
    fn span(&self) -> Span {
        match self {
            StatementKind::Expression(x) => x.span(),
            StatementKind::Declaration(x) => x.span(),
            StatementKind::Empty => Span::empty(),
        }
    }
}

pub fn parse_block_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    let group = consume_group(iter, OpenCurly)?;
    let mut iter = make_awesome(group.children.iter());

    token_starter!(semicolon_matcher, Semicolon);

    let mut block = BlockExpressionNode {
        span_: group.span(),
        statements: vec![],
        value: None,
    };

    let mut last_stmt_is_unterminated_expr = false;

    loop {
        let mut stmt = {
            let mut sub_iter = iter.until(semicolon_matcher);
            if let Some(let_decl) = parse_let_declaration(&mut sub_iter, errors) {
                StatementNode {
                    span_: let_decl.span(),
                    kind: StatementKind::Declaration(let_decl),
                }
            } else if let Some(expr) = parse_expression(&mut sub_iter, errors, false) {
                StatementNode {
                    span_: expr.span(),
                    kind: StatementKind::Expression(expr),
                }
            } else {
                StatementNode {
                    span_: Span::empty(),
                    kind: StatementKind::Empty,
                }
            }
        };

        if let Some(semicolon) = consume_token!(iter, Semicolon) {
            stmt.span_ += semicolon.span();
            block.statements.push(stmt);
            last_stmt_is_unterminated_expr = false;
        } else {
            match stmt.kind {
                StatementKind::Expression(expr) if expr.is_block() => {
                    block.statements.push(StatementNode {
                        span_: expr.span(),
                        kind: StatementKind::Expression(expr),
                    });
                    last_stmt_is_unterminated_expr = true;
                }
                StatementKind::Expression(expr) => {
                    block.value = Some(Box::new(expr));
                    last_stmt_is_unterminated_expr = false;
                    break;
                }
                StatementKind::Declaration(decl) => {
                    add_error!(errors, decl.span().end(), MissingSemicolon);
                    block.statements.push(StatementNode {
                        span_: decl.span(),
                        kind: StatementKind::Declaration(decl),
                    });
                }

                StatementKind::Empty => {
                    break;
                }
            }
        }
    }

    if last_stmt_is_unterminated_expr {
        block.value = match block.statements.pop() {
            Some(StatementNode {
                kind: StatementKind::Expression(expr),
                ..
            }) => Some(Box::new(expr)),
            _ => unreachable!(),
        };
    }

    // consume remaining tokens and generate error if there are any
    recover_until(&mut iter, errors, [], []);

    Some(ExpressionNode::Block(block))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::{ErrorKind, Errors};
    use crate::parser::literal_expression_node::{NumberLiteralNode, LiteralExpressionNode};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};
    use assert_matches::assert_matches;
    use crate::parser::add_expression_node::{AddExpressionNode, AddExpressionNodeFollow};

    #[test]
    fn empty_input() {
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
    fn unknown_input() {
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
        assert_eq!(
            remaining,
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn empty_block() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![],
                value: None,
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_just_value() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![],
                value: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                        60..70,
                        test_token!(DecInteger:60..70),
                        false
                    ))
                ))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_unexpected_token_after_value() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, Unknown:75..80 }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![],
                value: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                        60..70,
                        test_token!(DecInteger:60..70),
                        false
                    ))
                ))),
            }))
        );
        assert!(errors.has_error_at(75..80, ErrorKind::UnexpectedToken(Unknown)));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_single_expression_statement() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, DecInteger:60..70, Semicolon:75 }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![StatementNode {
                    span_: (60..76).into(),
                    kind: StatementKind::Expression(ExpressionNode::Literal(
                        LiteralExpressionNode::Number(NumberLiteralNode::new(
                            60..70,
                            test_token!(DecInteger:60..70),
                            false
                        ))
                    )),
                }],
                value: None,
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_nonblock_expression_statement_and_value() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!({:4, DecInteger:10..13, Semicolon:15, DecInteger:60..70 }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![StatementNode {
                    span_: (10..16).into(),
                    kind: StatementKind::Expression(ExpressionNode::Literal(
                        LiteralExpressionNode::Number(NumberLiteralNode::new(
                            10..13,
                            test_token!(DecInteger:10..13),
                            false
                        ))
                    )),
                }],
                value: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                        60..70,
                        test_token!(DecInteger:60..70),
                        false
                    ))
                ))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_block_expression_statement_and_value() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, {:10, }:15, DecInteger:60..70 }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![StatementNode {
                    span_: (10..16).into(),
                    kind: StatementKind::Expression(ExpressionNode::Block(BlockExpressionNode {
                        span_: (10..16).into(),
                        statements: vec![],
                        value: None,
                    })),
                }],
                value: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                        60..70,
                        test_token!(DecInteger:60..70),
                        false
                    ))
                ))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_block_expression_semicolon_statement_and_value() {
        // arrange
        let input: Vec<TokenTree> =
            test_tokentree!({:4, {:10, }:15, Semicolon:16, DecInteger:60..70 }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![StatementNode {
                    span_: (10..17).into(),
                    kind: StatementKind::Expression(ExpressionNode::Block(BlockExpressionNode {
                        span_: (10..16).into(),
                        statements: vec![],
                        value: None,
                    })),
                }],
                value: Some(Box::new(ExpressionNode::Literal(
                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                        60..70,
                        test_token!(DecInteger:60..70),
                        false
                    ))
                ))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_block_value() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, {:10,}:20, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![],
                value: Some(Box::new(ExpressionNode::Block(BlockExpressionNode {
                    span_: (10..21).into(),
                    statements: vec![],
                    value: None,
                }))),
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_single_let_statement() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, Let:10..13, Identifier:14..20, Equals:23, DecInteger:25..30, Semicolon:31, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert

        assert_matches!(result, Some(ExpressionNode::Block(BlockExpressionNode {
                span_: Span(4, 121),
                statements: stmts,
                value: None,
            })) => {
            assert_matches!(stmts[..], [
                StatementNode {
                    span_: Span(10, 32),
                    kind: StatementKind::Declaration(_),
                }
            ]);
        });

        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_single_let_statement_and_value() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, Let:10..13, Identifier:14..20, Equals:23, DecInteger:25..30, Semicolon:31, DecInteger:40, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert

        assert_matches!(result, Some(ExpressionNode::Block(BlockExpressionNode {
                span_: Span(4, 121),
                statements: stmts,
                value: Some(_),
            })) => {
            assert_matches!(stmts[..], [
                StatementNode {
                    span_: Span(10, 32),
                    kind: StatementKind::Declaration(_),
                }
            ]);
        });

        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn greedy_block_expr_after_statement() {
        // ensures that the until iterator is correctly used
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({
            DecInteger, Semicolon,
            DecInteger, Plus, {DecInteger}
        });
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);

        // assert

        assert!(errors.get_errors().is_empty());
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: Span::empty(),
                statements: vec![StatementNode {
                    span_: Span::empty(),
                    kind: StatementKind::Expression(ExpressionNode::Literal(
                        LiteralExpressionNode::Number(NumberLiteralNode::new(
                            Span::empty(),
                            test_token!(DecInteger),
                            false
                        ))
                    )),
                }],
                value: Some(Box::new(ExpressionNode::Add(AddExpressionNode::new(
                    Span::empty(),
                    Box::new(ExpressionNode::Literal(
                        LiteralExpressionNode::Number(NumberLiteralNode::new(
                            Span::empty(),
                            test_token!(DecInteger),
                            false
                        ))
                    )),
                    vec![
                        AddExpressionNodeFollow {
                            operator: test_token!(Plus),
                            operand:Some(ExpressionNode::Block(BlockExpressionNode::new(
                                Span::empty(),
                                vec![],
                                Some(Box::new(ExpressionNode::Literal(
                                    LiteralExpressionNode::Number(NumberLiteralNode::new(
                                        Span::empty(),
                                        test_token!(DecInteger),
                                        false
                                    ))
                                )))
                            )))
                        }
                    ]


                )))),
            }))
        );
    }

    #[test]
    fn block_with_let_missing_semicolon() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, Let:10..13, Identifier:14..20, Equals:23, DecInteger:25..30, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert

        assert_matches!(result, Some(ExpressionNode::Block(BlockExpressionNode {
                span_: Span(4, 121),
                statements: stmts,
                value: None,
            })) => {
            assert_matches!(stmts[..], [
                StatementNode {
                    span_: Span(10, 30),
                    kind: StatementKind::Declaration(_),
                }
            ]);
        });

        assert!(errors.has_error_at(30, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 1);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
    #[test]
    fn block_with_let_missing_semicolon_and_another_let() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4,
            Let:10..13, Identifier:14..20, Equals:23, DecInteger:25..30,
            Let:40..43, Identifier:45..50, Equals:52, DecInteger:55..56,
        }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert

        assert_matches!(result, Some(ExpressionNode::Block(BlockExpressionNode {
                span_: Span(4, 121),
                statements: stmts,
                value: None,
            })) => {
            assert_matches!(stmts[..], [
                StatementNode {
                    span_: Span(10, 30),
                    kind: StatementKind::Declaration(_),
                },
                StatementNode {
                    span_: Span(40, 56),
                    kind: StatementKind::Declaration(_),
                },
            ]);
        });

        assert!(errors.has_error_at(30, ErrorKind::MissingSemicolon));
        assert!(errors.has_error_at(56, ErrorKind::MissingSemicolon));
        assert_eq!(errors.get_errors().len(), 2);
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_expr_after_block() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4,
            {:35, }:36,
            Minus:39, DecInteger:40, Plus:41, DecInteger:42, Semicolon:43,
        }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_matches!(result, Some(ExpressionNode::Block(BlockExpressionNode {
                span_: _,
                statements: stmts,
                value: None,
            })) => {
            assert_matches!(stmts[..], [
                StatementNode {
                    span_: Span(35, 37),
                    kind: StatementKind::Expression(ExpressionNode::Block(_)),
                },
                StatementNode {
                    span_: Span(39, 44),
                    kind: StatementKind::Expression(ExpressionNode::Add(_)),
                },
            ]);
        });

        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn block_with_multiple_statements_and_value() {}

    #[test]
    fn block_with_empty_statements() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!({:4, Semicolon:10, Semicolon:20, {:30, }:31, Semicolon:40, Semicolon:50, }:120);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_block_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(
            result,
            Some(ExpressionNode::Block(BlockExpressionNode {
                span_: (4..121).into(),
                statements: vec![
                    StatementNode {
                        span_: 10.into(),
                        kind: StatementKind::Empty,
                    },
                    StatementNode {
                        span_: 20.into(),
                        kind: StatementKind::Empty,
                    },
                    StatementNode {
                        span_: (30..41).into(),
                        kind: StatementKind::Expression(ExpressionNode::Block(
                            BlockExpressionNode {
                                span_: (30..32).into(),
                                statements: vec![],
                                value: None,
                            }
                        ))
                    },
                    StatementNode {
                        span_: 50.into(),
                        kind: StatementKind::Empty,
                    },
                ],
                value: None,
            }))
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}
