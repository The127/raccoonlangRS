use std::collections::HashMap;
use crate::ast::typing::TypeRef;
use crate::ir::function::{Function, FunctionSignature};
use crate::ir::ids::{SignatureId, TypeId};
use crate::ir::package_ir_builder::FunctionId;

#[derive(Debug)]
pub struct Package {
    pub functions: Vec<Function>,
    pub tuple_types: HashMap<Vec<TypeId>, TypeId>,
    pub signatures: HashMap<FunctionSignature, SignatureId>,
    pub signatures_reverse: HashMap<SignatureId, FunctionSignature>,
    // TODO: public functions end up in the package's export list
}

impl Package {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            tuple_types: HashMap::new(),
            signatures: HashMap::new(),
            signatures_reverse: HashMap::new(),
        }
    }

    pub fn get_function(&self, id: FunctionId) -> &Function {
        self.functions.get(id.0).unwrap()
    }

    pub fn get_signature(&self, id: SignatureId) -> FunctionSignature {
        if id == SignatureId::empty() {
            return FunctionSignature {
                parameters: vec![],
                return_: TypeId::unit(),
            }
        }
        self.signatures_reverse[&id].clone()
    }
}