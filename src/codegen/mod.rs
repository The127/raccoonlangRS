use cranelift_codegen::entity::EntityRef;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_codegen::ir::{UserFuncName, Function, Signature, AbiParam};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::InstBuilder;

pub fn gen_function() {
    let mut ctx = FunctionBuilderContext::new();
    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(I32));
    sig.returns.push(AbiParam::new(I32));
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    let mut builder = FunctionBuilder::new(&mut func, &mut ctx);
    let block0 = builder.create_block();
    builder.append_block_params_for_function_params(block0);
    builder.switch_to_block(block0);
    builder.seal_block(block0);

    let func_param = Variable::new(0);
    builder.declare_var(func_param, I32);
    builder.def_var(func_param, builder.block_params(block0)[0]);

    let two = Variable::new(1);
    builder.declare_var(two, I32);
    {
        let temp = builder.ins().iconst(I32, 2);
        builder.def_var(two, temp);
    }

    let result = Variable::new(2);
    builder.declare_var(result, I32);

    {
        let func_param_use = builder.use_var(func_param);
        let two_use = builder.use_var(two);
        let temp = builder.ins().iadd(func_param_use, two_use);
        builder.def_var(result, temp);
    }

    {
        let temp = builder.use_var(result);
        builder.ins().return_(&[temp]);
    }


    builder.finalize();

    println!("{}", func.display());
}