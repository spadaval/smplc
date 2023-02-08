use std::fmt::{format, Write};

use super::ssa::{self, BasicBlock, BasicBlockData, ControlFlowGraph, InstructionKind};

enum EdgeKind {
    Goto,
    Conditional(String),
}
pub struct Graph {
    blocks: Vec<Block>,
    // source, target, label (optional)
    // yes, I know this is bad . Shut up.
    edges: Vec<(usize, usize, EdgeKind)>,
}

impl Graph {
    pub fn new(cfg: ssa::ControlFlowGraph) -> Self {
        let blocks = cfg.blocks().collect::<Vec<(BasicBlock, BasicBlockData)>>();
        let mut edges = Vec::new();
        for (block, block_data) in &blocks {
            if let Some(x) = &block_data.terminator {
                match x {
                    ssa::Terminator::Goto(target) => {
                        edges.push((block.0, target.0, EdgeKind::Goto))
                    }
                    ssa::Terminator::ConditionalBranch {
                        condition,
                        target,
                        fallthrough,
                    } => {
                        // TODO include condition details in graph
                        edges.push((
                            block.0,
                            target.0,
                            EdgeKind::Conditional("target".to_string()),
                        ));
                        edges.push((
                            block.0,
                            fallthrough.0,
                            EdgeKind::Conditional("fallthrough".to_string()),
                        ));
                    }
                }
            }
        }

        let blocks = blocks
            .iter()
            .map(|(id, bb_data)| Block::new(id, bb_data))
            .collect();
        Graph { blocks, edges }
    }

    pub fn render(&self) -> String {
        let mut dot = String::new();
        writeln!(dot, "digraph {{").unwrap();

        for block in &self.blocks {
            block.write_record(&mut dot).unwrap();
        }

        //bb1:b -> bb2:b [color=blue, style=dotted, label="dom"]
        for (start, end, kind) in &self.edges {
            match kind {
                EdgeKind::Goto => writeln!(dot, "bb{}:s -> bb{}:n", start, end).unwrap(),
                EdgeKind::Conditional(s) => {
                    writeln!(dot, "bb{}:s -> bb{}:n [label=\"{}\" ]", start, end, s).unwrap()
                }
            };
        }

        write!(dot, "}}").unwrap();

        dot
    }
}

struct Block {
    id: usize,
    header: Vec<Instruction>,
    instructions: Vec<Instruction>,
    terminator: Option<ssa::Terminator>,
}

impl Block {
    fn new(block: &ssa::BasicBlock, block_data: &ssa::BasicBlockData) -> Self {
        let header = block_data
            .header
            .iter()
            .map(|it| Instruction::from(it))
            .collect();
        let instructions = block_data
            .statements
            .iter()
            .map(|it| Instruction::new(it))
            .collect();

        //let terminator: Instruction = block_data.terminator.into();

        Block {
            id: block.0,
            header,
            instructions,
            terminator: block_data.terminator.clone(),
        }
    }

    fn write_record(&self, str: &mut String) -> Result<(), std::fmt::Error> {
        write!(str, "bb{} ", self.id).unwrap();
        let mut label = format!("<b>BB{} | ", self.id);
        write!(label, "{{").unwrap();
        let ins = self
            .instructions
            .iter()
            .map(|it| it.to_string())
            .collect::<Vec<String>>()
            .join(" | ");

        write!(label, "{ins}").unwrap();

        write!(label, "}}").unwrap();

        writeln!(str, "[shape=record, label=\"{}\"];", label)
    }
}

enum Op {
    Add,
    AddI,
    Const,
}

enum Operand {
    InstructionReference(usize),
    Constant(f32),
}
struct Instruction {
    id: usize,
    op: String,
    operands: Vec<Operand>,
}

impl Instruction {
    fn new(instruction: &ssa::Instruction) -> Self {
        match instruction.kind.clone() {
            ssa::InstructionKind::Constant(c) => Instruction {
                id: instruction.id.0,
                op: "Const".to_string(),
                operands: vec![Operand::Constant(c)],
            },
            ssa::InstructionKind::BasicOp(op, l, r) => Instruction {
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![
                    Operand::InstructionReference(l.0),
                    Operand::InstructionReference(r.0),
                ],
            },
            ssa::InstructionKind::ImmediateOp(_, _, _) => todo!(),
        }
    }

    fn to_string(&self) -> String {
        //6: add (2) (2)
        let mut s = String::new();
        write!(s, "{}: {}", self.id, self.op).unwrap();
        for op in &self.operands {
            match op {
                Operand::InstructionReference(ins) => write!(s, " ({})", ins).unwrap(),
                Operand::Constant(c) => write!(s, " #{}", c).unwrap(),
            };
        }
        s
    }
}

impl Into<String> for InstructionKind {
    fn into(self) -> String {
        match self {
            InstructionKind::Constant(_) => "Const".to_string(),
            InstructionKind::BasicOp(op, _, _) => match op {
                ssa::BasicOpKind::Add => "add".to_string(),
                ssa::BasicOpKind::Subtract => "sub".to_string(),
            },
            InstructionKind::ImmediateOp(_, _, _) => todo!(),
        }
    }
}

impl From<&ssa::HeaderStatement> for Instruction {
    fn from(instruction: &ssa::HeaderStatement) -> Self {
        match instruction {
            _ => todo!(),
        }
    }
}

impl From<ssa::Terminator> for Instruction {
    fn from(instruction: ssa::Terminator) -> Self {
        match instruction {
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse, ssa::ssa::lower_program};

    #[test]
    fn test_if_dot() {
        let program = r"
            let a <- 0;
            let b <- 10;
            if a < b
            then 
                let b <- b + a;
            else 
                let a <- a + b;
            fi
        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let cfg = lower_program(forest);
        let g = Graph::new(cfg);
        println!("{}", g.render());
    }
}
