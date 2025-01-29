use crate::ast::expressions::Expression;
use crate::ast::parse_transform::access::transform_access_expression;
use crate::ast::parse_transform::binary::{transform_add_expression, transform_compare_expression, transform_mul_expression};
use crate::ast::parse_transform::block::transform_block_expression;
use crate::ast::parse_transform::if_::transform_if_expression;
use crate::ast::parse_transform::literal::transform_literal_expression;
use crate::ast::parse_transform::subsequent::transform_subsequent_expression;
use crate::ast::parse_transform::tuple::transform_tuple_expression;
use crate::errors::Errors;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::SourceCollection;

pub mod access;
pub mod binary;
pub mod block;
pub mod if_;
pub mod literal;
mod subsequent;
pub mod tuple;
pub mod file;
mod function;
mod pattern;
mod statement;
mod struct_;
mod type_;

pub fn transform_expression(
    node: &ExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    match node {
        ExpressionNode::Literal(x) => transform_literal_expression(x, errors, sources),
        ExpressionNode::Block(x) => transform_block_expression(x, errors, sources),
        ExpressionNode::If(x) => transform_if_expression(x, errors, sources),
        ExpressionNode::Add(x) => transform_add_expression(x, errors, sources),
        ExpressionNode::Mul(x) => transform_mul_expression(x, errors, sources),
        ExpressionNode::Compare(x) => transform_compare_expression(x, errors, sources),
        ExpressionNode::Access(x) => transform_access_expression(x, errors, sources),
        ExpressionNode::Tuple(x) => transform_tuple_expression(x, errors, sources),
        ExpressionNode::Subsequent(x) => transform_subsequent_expression(x, errors, sources),
        ExpressionNode::New(x) => todo!()
    }
}

#[cfg(test)]
mod test {}
