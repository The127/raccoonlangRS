use crate::ir::function::{Function, Instruction};
use crate::ir::function_ir_builder::BlockId;

pub struct FunctionGraph {
    pub entry: BlockId,
    pub edges: Vec<(BlockId, BlockId)>,
    pub exits: Vec<BlockId>,
}

pub fn generate_function_graph(function: &Function) -> FunctionGraph {
    let mut graph = FunctionGraph {
        entry: BlockId(0),
        edges: vec![],
        exits: vec![],
    };
    for (idx, block) in function.blocks.iter().enumerate() {
        let last_instruction = block.instructions.last().unwrap();
        match last_instruction {
            Instruction::Branch(dest) => {
                graph.edges.push((BlockId(idx), *dest));
            }
            Instruction::BranchIf(_, dest1, dest2) => {
                graph.edges.push((BlockId(idx), *dest1));
                graph.edges.push((BlockId(idx), *dest2));
            }
            Instruction::Return(_) => {
                graph.exits.push(BlockId(idx));
            }
            _ => panic!("invalid block, last instruction is {:?}", last_instruction),
        }
    }

    graph
}

#[mutants::skip]
pub fn generate_dot(function: &Function, graph: &FunctionGraph) -> String {
    let mut dot = "digraph G {\n".to_string();
    dot += "  ordering=out;\n";
    dot += r#"  graph[fontsize=10 fontname="Verdana"];"#;
    dot += "\n";
    dot += r##"  node[shape=box style=filled fontsize=8 fontname="Verdana" fillcolor="#efefef"];"##;
    dot += "\n";

    dot += r#"  node_entry [shape="point"];"#;
    dot += "\n";


    for (idx, block) in function.blocks.iter().enumerate() {
        let mut instructions_str = "".to_string();

        for instr in &block.instructions {
            let instr_text = format!("{}", instr)
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;");

            instructions_str += format!(r#"<TR><TD ALIGN="LEFT" BORDER="0" CELLPADDING="0" CELLSPACING="0">{}</TD></TR>"#, instr_text).as_str();
        }

        dot += format!(r#"  node_{} [label=< <TABLE BORDER="0"><TR><TD ALIGN="LEFT" BORDER="0" CELLPADDING="0" CELLSPACING="0"><B>Block {}</B></TD></TR>{}</TABLE> >];"#, idx, idx, instructions_str).as_str();
        dot += "\n";
    }

    dot += r#"  node_exit [shape="point"];"#;
    dot += "\n";

    dot += "\n";

    dot += format!("  node_entry -> node_{};\n", graph.entry.0).as_str();

    for (from, to) in &graph.edges {
        dot += format!("  node_{} -> node_{};\n", from.0, to.0).as_str();
    }

    for id in &graph.exits {
        dot += format!("  node_{} -> node_exit;\n", id.0).as_str();
    }

    dot += "}";

    dot
}