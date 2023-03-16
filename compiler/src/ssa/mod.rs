mod cfg;
mod replacer;
mod ssa;
mod ssa_render;
mod types;

use crate::parser::Function;
use ssa::lower_block;

pub fn lower_function(function: &Function) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    let block = cfg.start_block();
    for var in &function.variables {
        let id = cfg.add_header_statement(block, types::HeaderStatementKind::Param(var.ident()));
        cfg.set_symbol(block, var.ident(), id);
    }
    let func_start = cfg.new_block(block);
    cfg.goto(block, func_start);
    lower_block(&mut cfg, func_start, &function.body);

    cfg
}

pub fn lower_program(forest: Program) -> Vec<ControlFlowGraph> {
    forest
        .functions
        .iter()
        .map(|func| lower_function(func))
        .collect()
}

pub use ssa_render::render_program;
pub use ssa_render::FunctionGraph;

use crate::parser::Program;

use self::cfg::ControlFlowGraph;