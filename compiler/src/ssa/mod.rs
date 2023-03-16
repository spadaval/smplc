mod cfg;
mod replacer;
mod ssa;
mod ssa_render;
mod types;

pub use ssa::lower_function;

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
