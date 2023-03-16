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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{parser::parse, ssa::ssa_render::render_program, SourceFile};

    use super::render_program as do_test;

    #[test]
    fn test_func() {
        let program = r"
            main
            var a; {
                let n <- call InputNum();
                let fib <- call fibbonachi(n);
                call OutputNum(fib);
            }
            fibbonachi(n) {
                let a <- 0;
                let b <- 1;
                let i <- 0;
                while i < n
                do 
                    let i <- i + 1
                    let sum <- a + b;
                    let a <- b;
                    let b <- sum;
                od
                return b;
            }

        ";
        do_test(program);
    }

    #[test]
    fn test_cse() {
        let program = r"
            main {
                let a <- 1+2-3+4-6;
                let b <- 1+2-3+4-6;
                let c <- 1+2-3+4-6;
                while x < 1 do
                    let z <- 1+7;
                    call OutputNum(z);
                od
            }
        ";
        do_test(program);
    }

    #[test]
    fn test_bubble_sort() {
        let program = r"
            main 
            array[4] a; 
            {
                call bubble_sort(a, 4);
            }

            function bubble_sort(arr, size) {
                let i <- 0;
                while i < size do
                    let j <- 0;
                    while j < size - i - 1 do 
                        if arr[j] > arr[j+1] then
                            let temp <- arr[j]
                            let arr[j] <- arr[j+1]
                            let arr[j+1] <- temp
                        fi
                        let i <- i + 1;
                    let j <- j+1
                    od
                od
            }
        ";
        do_test(program);
    }

    #[test]
    fn test_while() {
        let program = "
        main
var i, x, y, j;
{
    let i <- call InputNum();
    let x <- 0;
    let y <- 0;
    let j <- i;
    while x < 10 do
        let x <- i + 1;
        let x <- x + 1;
        let y <- j + 1;
        let i <- i + 1;
    od;
    call OutputNum(i);
    call OutputNum(x);
    call OutputNum(j);
    call OutputNum(y)
}.
        ";
        do_test(program);
    }
}
