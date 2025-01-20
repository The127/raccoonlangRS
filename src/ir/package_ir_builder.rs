use crate::ast::typing::{BuiltinType, TypeRef};
use crate::ir::function::{Function, FunctionSignature};
use crate::ir::function_ir_builder::FunctionIrBuilder;
use crate::ir::ids::{SignatureId, TypeId};
use crate::ir::package::Package;

#[derive(Debug)]
pub struct PackageIrBuilder<'a> {
    pub(super) package: &'a mut Package,
    next_type_id: usize,
    next_signature_id: usize,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct FunctionId(pub(super) usize);

impl<'a> PackageIrBuilder<'a> {
    pub fn new(package: &mut Package) -> PackageIrBuilder {
        PackageIrBuilder {
            package,
            next_type_id: 0,
            next_signature_id: 1, // starts at 1  because 0 is SignatureId::empty()
        }
    }

    pub fn create_function(&mut self) -> FunctionId {
        let idx = self.package.functions.len();
        self.package.functions.push(Function::new());

        FunctionId(idx)
    }

    pub fn function_builder<'b>(&'b mut self, id: FunctionId) -> FunctionIrBuilder<'b, 'a> {
        FunctionIrBuilder::new(self, id)
    }

    pub fn map_type(&mut self, type_ref: &TypeRef) -> TypeId {
        match type_ref {
            TypeRef::Unknown => unreachable!(),
            TypeRef::Builtin(x) => match x {
                BuiltinType::Unit => TypeId::unit(),
                BuiltinType::Bool => TypeId::bool(),
                BuiltinType::I32 => TypeId::i32(),
                BuiltinType::U32 => todo!(),
                BuiltinType::F32 => todo!(),
            },
            TypeRef::Tuple(t) => {
                let field_types = t.fields.iter().map(|x| self.map_type(x)).collect();
                let tuple_type_id =
                    self.package
                        .tuple_types
                        .entry(field_types)
                        .or_insert_with(|| {
                            let id = self.next_type_id;
                            self.next_type_id += 1;
                            TypeId::new(id)
                        });

                *tuple_type_id
            }
        }
    }

    pub fn map_signature(&mut self, return_: TypeId, params: Vec<TypeId>) -> SignatureId {
        if params.is_empty() && return_ == TypeId::unit() {
            return SignatureId::empty();
        }

        let sig = FunctionSignature {
            parameters: params,
            return_,
        };
        let id = *self.package.signatures.entry(sig.clone()).or_insert_with(|| {
            let id = self.next_signature_id;
            self.next_signature_id += 1;
            SignatureId::new(id)
        });
        self.package.signatures_reverse.entry(id).or_insert(sig);

        id
    }
}

#[cfg(test)]
mod test {
    use crate::ast::typing::{BuiltinType, TypeRef};
    use crate::ir::function::{Function, FunctionSignature};
    use crate::ir::ids::{SignatureId, TypeId};
    use crate::ir::package::Package;
    use crate::ir::package_ir_builder::{FunctionId, PackageIrBuilder};
    use std::collections::HashMap;

    #[test]
    fn new() {
        // arrange
        let mut package = Package::new();

        // act
        let builder = PackageIrBuilder::new(&mut package);

        // assert
        assert_eq!(builder.next_type_id, 0);
        assert!(package.functions.is_empty());
    }

    #[test]
    fn new_func() {
        // arrange
        let mut package = Package::new();
        let mut builder = PackageIrBuilder::new(&mut package);

        // act
        let func_id = builder.create_function();

        // assert
        assert_eq!(func_id, FunctionId(0));
        assert_eq!(package.functions, vec![Function::new(),])
    }

    #[test]
    fn func_builder() {
        // arrange
        let mut package = Package::new();
        let mut builder = PackageIrBuilder::new(&mut package);
        let func_1 = builder.create_function();
        let func_2 = builder.create_function();

        // act
        let v1 = {
            let mut func_builder = builder.function_builder(func_1);
            func_builder.create_local(TypeId::bool())
        };


        let v2 = {
            let mut func_builder = builder.function_builder(func_2);
            func_builder.create_local(TypeId::i32())
        };

        // assert
        assert_eq!(package.functions.len(), 2);
        assert_eq!(package.functions[0].locals, vec![(v1, TypeId::bool())]);
        assert_eq!(package.functions[1].locals, vec![(v2, TypeId::i32())]);
    }

    #[test]
    fn map_type() {
        // arrange
        let mut package = Package::new();
        let mut builder = PackageIrBuilder::new(&mut package);

        let typeref_i32 = TypeRef::Builtin(BuiltinType::I32);
        let typeref_bool = TypeRef::Builtin(BuiltinType::Bool);
        let typeref_tuple1 = TypeRef::tuple(vec![typeref_i32.clone(), typeref_i32.clone()]);
        let typeref_tuple2 = TypeRef::tuple(vec![typeref_i32.clone(), typeref_bool.clone()]);

        // act
        let t_i32 = builder.map_type(&typeref_i32);
        let t_bool = builder.map_type(&typeref_bool);
        let t_tuple1a = builder.map_type(&typeref_tuple1);
        let t_tuple1b = builder.map_type(&typeref_tuple1);
        let t_tuple2 = builder.map_type(&typeref_tuple2);

        // assert
        assert_eq!(t_i32, TypeId::i32());
        assert_eq!(t_bool, TypeId::bool());
        assert_eq!(t_tuple1a, t_tuple1b);
        assert_ne!(t_tuple1a, t_tuple2);
        assert_eq!(
            package.tuple_types,
            HashMap::from([
                (vec![t_i32, t_i32], t_tuple1a),
                (vec![t_i32, t_bool], t_tuple2),
            ]),
        );
    }

    #[test]
    fn map_empty_signature() {
        // arrange
        let mut package = Package::new();
        let mut builder = PackageIrBuilder::new(&mut package);

        let t_unit = TypeId::unit();

        // act
        let sig_id = builder.map_signature(t_unit, vec![]);
        let signature = package.get_signature(sig_id);

        // assert
        assert_eq!(sig_id, SignatureId::empty());
        assert_eq!(signature, FunctionSignature {
            parameters: vec![],
            return_: t_unit,
        });
    }

    #[test]
    fn map_signature() {
        // arrange
        let mut package = Package::new();
        let mut builder = PackageIrBuilder::new(&mut package);

        let t_i32 = TypeId::i32();
        let t_bool = TypeId::bool();

        // act
        let sig_id = builder.map_signature(t_bool, vec![t_i32, t_i32]);
        let signature = package.get_signature(sig_id);

        // assert
        assert_eq!(sig_id, SignatureId::new(1));
        assert_eq!(signature, FunctionSignature {
            parameters: vec![t_i32, t_i32],
            return_: t_bool,
        });
    }
}
