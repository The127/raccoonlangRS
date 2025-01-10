use crate::ast::expressions::{transform_expression, Expression};
use crate::parser::block_expression_node::{StatementKind, StatementNode};
use crate::parser::expression_node::ExpressionNode;
use crate::parser::let_declaration_node::LetDeclarationNode;
use crate::source_map::{HasSpan, SourceCollection, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Expression(Expression),
    Declaration(LetDeclaration),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetDeclaration {
    span_: Span,
    pub binding: Option<Ustr>,
    pub value: Option<Expression>,
}

impl HasSpan for LetDeclaration {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_statement(node: &StatementNode, sources: &SourceCollection) -> Option<Statement> {
    match &node.kind {
        StatementKind::Empty => None,
        StatementKind::Expression(x) => Some(transform_expression_statement(x, sources)),
        StatementKind::Declaration(x) => Some(transform_declaration_statement(x, sources)),
    }
}

fn transform_expression_statement(node: &ExpressionNode, sources: &SourceCollection) -> Statement {
    Statement::Expression(transform_expression(node, sources))
}

fn transform_declaration_statement(
    node: &LetDeclarationNode,
    sources: &SourceCollection,
) -> Statement {
    Statement::Declaration(LetDeclaration {
        span_: node.span(),
        binding: node.binding.map(|t| sources.get_identifier(t.span())),
        value: node.value.as_ref().map(|e| transform_expression(e, sources)),
    })
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::statement::{transform_statement, LetDeclaration, Statement};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::block_expression_node::StatementNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::let_declaration_node::LetDeclarationNode;
    use crate::source_map::{SourceCollection, Span};
    use crate::test_token;
    use crate::tokenizer::TokenType::Identifier;
    use ustr::ustr;

    #[test]
    fn transform_empty() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(";");
        let stmt_node = StatementNode::empty(span);

        // act
        let stmt = transform_statement(&stmt_node, &sources);

        // assert
        assert_eq!(stmt, None);
    }

    #[test]
    fn transform_expression() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foobar");
        let stmt_node = StatementNode::expr(span, ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span))));

        // act
        let stmt = transform_statement(&stmt_node, &sources);

        // assert
        assert_eq!(stmt, Some(Statement::Expression(Expression::access(span, ustr("foobar")))));
    }

    #[test]
    fn transform_let_decl() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("let foo = bar;");
        let binding_span = Span(span.start() + 4, span.start() + 7);
        let access_span = Span(span.start() + 10, span.start() + 13);

        let stmt_node = StatementNode::decl(
            span,
            LetDeclarationNode::new(
                span,
                Some(test_token!(Identifier:binding_span)),
                Some(ExpressionNode::Access(AccessExpressionNode::new(
                    test_token!(Identifier:access_span),
                ))),
            ),
        );

        // act
        let stmt = transform_statement(&stmt_node, &sources);

        // assert
        assert_eq!(
            stmt,
            Some(Statement::Declaration(LetDeclaration {
                span_: span,
                binding: Some(ustr("foo")),
                value: Some(Expression::access(access_span, ustr("bar"))),
            }))
        );
    }
}
