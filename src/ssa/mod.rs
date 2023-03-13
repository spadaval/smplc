mod cfg;
mod replacer;
mod ssa;
mod ssa_render;
mod types;

use ssa::lower_block;

pub fn lower_program(forest: ProgramForest) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    let block = cfg.start_block();
    lower_block(&mut cfg, block, &forest.roots[0]);
    cfg
}

pub use ssa_render::render_program;
pub use ssa_render::Graph;

use crate::parser::ProgramForest;

use self::cfg::ControlFlowGraph;

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_env_logger::env_logger;

    use crate::{parser::parse, ssa::ssa_render::render_program};

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_assign() {
        let forest = parse(crate::SourceFile::new("let a <- a + 1"));
        let cfg = lower_program(forest);
        println!("{cfg:#?}");
    }

    #[test]
    fn test_if() {
        init();
        let program = r"
            let a <- 0;
            let b <- 10;
            if a < b
            then 
                let b <- b + a;
            fi
        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let _cfg = lower_program(forest);
        //println!("{cfg:#?}");
        //cfg.render();
    }

    #[test]
    fn test_dot_fibonacci() {
        pretty_env_logger::init();

        let program = r"
            let n <- call InputNum();
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
            
            call OutputNum(b);
        ";
        let dot = render_program(program.to_owned());

        println!("{}", dot);
    }

    #[test]
    fn test_dot_nested() {
        pretty_env_logger::init();

        // prints multiples of 10 until n
        let program = r"
            let n <- call InputNum();
            let a <- 0;
            let b <- 1;
            let i <- 0;
            while i < n
            do       

                let j <- 0;
                while j < 10
                do
                    let i <- i+1;
                    let j <- i+1;
                od
                
                call OutputNum(i);
            od
            
            call OutputNum(b);
        ";
        let dot = render_program(program.to_owned());

        println!("{}", dot);
    }
}
