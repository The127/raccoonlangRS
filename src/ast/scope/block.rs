use std::rc::Rc;
use crate::ast::expressions::block::BlockExpression;
use crate::ast::scope::Scope;
use crate::ast::typing::{BuiltinType, TypeRef};
use ustr::{ustr, Ustr};
use crate::ast::function_decl::FunctionDecl;
use crate::ast::scope::function::FunctionScope;

pub struct BlockScope<'a> {
    parent: &'a dyn Scope,
    decl_name: Ustr,
    decl_type: TypeRef,
}

impl BlockScope<'_> {
    pub fn new<'a >(parent: &'a dyn Scope, expr: &BlockExpression) -> BlockScope<'a> {
        let decl = expr
            .let_
            .as_ref()
            .expect("only create BlockScope for blocks that have a decl");
        BlockScope {
            parent,
            decl_name: decl.binding,
            decl_type: decl.type_ref.clone().unwrap(),
        }
    }
}

impl Scope for BlockScope<'_> {
    fn lookup(&self, path: Vec<Ustr>, rooted: bool) -> Option<&TypeRef> {
        assert!(!rooted);
        assert_eq!(path.len(), 1);
        let name = path[0];

        if self.decl_name == name {
            Some(&self.decl_type)
        } else {
            self.parent.lookup(path, rooted)
        }
    }

    fn function(&self, func: &FunctionDecl) -> FunctionScope {
        todo!()
    }

    fn block(&self, expr: &BlockExpression) -> BlockScope {
        BlockScope::new(self, expr)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::block::LetDeclaration;
    use crate::ast::expressions::{Expression, ExpressionKind};
    use crate::ast::scope::global::GlobalScope;
    use crate::ast::typing::BuiltinType;
    use assert_matches::assert_matches;
    use parameterized::{ide, parameterized};
    use ustr::ustr;

    ide!();

    #[parameterized(params = {
        (ustr("foo"), Some(TypeRef::Builtin(BuiltinType::I32))),
        (ustr("bar"), None)
    })]
    fn decl_var(params: (Ustr, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let global_scope = GlobalScope::new();
        let expr = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1)))
                .with_type_ref(TypeRef::Builtin(BuiltinType::I32)),
            vec![],
            None,
        );
        let block_expr = assert_matches!(&expr.kind, ExpressionKind::Block(x) => x);

        // act
        let block_scope = global_scope.block(block_expr);
        let got_type = block_scope.lookup(vec![name], false);

        // assert
        assert_eq!(got_type, expected_type.as_ref());
    }

    #[parameterized(params = {
        (ustr("foo"), Some(TypeRef::Builtin(BuiltinType::I32))),
        (ustr("bar"), Some(TypeRef::Builtin(BuiltinType::Bool))),
        (ustr("qux"), None),
    })]
    fn nested_block(params: (Ustr, Option<TypeRef>)) {
        let (name, expected_type) = params;
        // arrange
        let global_scope = GlobalScope::new();
        let expr1 = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("foo"), Some(Expression::int_literal(0, 1)))
                .with_type_ref(TypeRef::Builtin(BuiltinType::I32)),
            vec![],
            None,
        );
        let block_expr_1 = assert_matches!(&expr1.kind, ExpressionKind::Block(x) => x);

        let expr2 = Expression::block_with_decl(
            0,
            false,
            LetDeclaration::new(0, ustr("bar"), Some(Expression::bool_literal(0, false)))
                .with_type_ref(TypeRef::Builtin(BuiltinType::Bool)),
            vec![],
            None,
        );
        let block_expr_2 = assert_matches!(&expr2.kind, ExpressionKind::Block(x) => x);


        // act
        let block_scope_1 = global_scope.block(block_expr_1);
        let x: &dyn Scope = &block_scope_1;
        let block_scope_2 = x.block(block_expr_2);
        let got_type = block_scope_2.lookup(vec![name], false);

        // assert
        assert_eq!(got_type, expected_type.as_ref());

    }
}
