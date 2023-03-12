use std::{collections::HashMap, fmt::Write};

use super::ssa::{self, BasicBlockData, BlockId, ControlFlowGraph, InstructionId, InstructionKind};

enum EdgeKind {
    Goto,
    Conditional(String),
    SymbolTable,
}

enum Direction {
    Up,
    Down,
}
struct Edge {
    start: String,
    end: String,
    kind: EdgeKind,
    direction: Direction,
}
impl Edge {
    fn bb(start: usize, end: usize, kind: EdgeKind) -> Self {
        Edge {
            start: format!("bb{start}"),
            end: format!("bb{end}"),
            kind,
            direction: if end > start {
                Direction::Down
            } else {
                Direction::Up
            },
        }
    }
    fn new(start: String, end: String, kind: EdgeKind) -> Self {
        Edge {
            start,
            end,
            kind,
            direction: Direction::Down,
        }
    }
}
pub struct Graph {
    blocks: Vec<Block>,
    // source, target, label (optional)
    // yes, I know this is bad . Shut up.
    edges: Vec<Edge>,
}

impl Graph {
    pub fn new(cfg: ControlFlowGraph) -> Self {
        let blocks = cfg.blocks().collect::<Vec<(BlockId, BasicBlockData)>>();
        let mut edges = Vec::new();
        for (block, block_data) in &blocks {
            if let Some(x) = &block_data.terminator {
                match x {
                    ssa::Terminator::Goto(target) => {
                        edges.push(Edge::bb(block.0, target.0, EdgeKind::Goto))
                    }
                    ssa::Terminator::ConditionalBranch {
                        condition: _,
                        target,
                        fallthrough,
                    } => {
                        // TODO include condition details in graph
                        edges.push(Edge::bb(
                            block.0,
                            target.0,
                            EdgeKind::Conditional("true".to_string()),
                        ));
                        edges.push(Edge::bb(
                            block.0,
                            fallthrough.0,
                            EdgeKind::Conditional("false".to_string()),
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

    pub fn render(&mut self) -> String {
        let mut dot = String::new();
        writeln!(dot, "digraph {{").unwrap();

        for block in &self.blocks {
            block.write_record(&mut dot).unwrap();
            let edge = block.write_symbols(&mut dot).unwrap().unwrap();
            self.edges.push(edge);
        }

        //bb1:b -> bb2:b [color=blue, style=dotted, label="dom"]
        for Edge {
            start,
            end,
            kind,
            direction,
        } in &self.edges
        {
            match kind {
                EdgeKind::Goto => match direction {
                    Direction::Up => {
                        writeln!(dot, "{start}:s -> {end}:e [label=\"goto\" ]").unwrap()
                    }
                    Direction::Down => {
                        writeln!(dot, "{start}:s -> {end}:n [label=\"goto\" ]").unwrap()
                    }
                },

                EdgeKind::Conditional(s) => match direction {
                    Direction::Up => {
                        writeln!(dot, "{start}:s -> {end}:e [label=\"{s}\" ]").unwrap()
                    }
                    Direction::Down => {
                        writeln!(dot, "{start}:s -> {end}:n [label=\"{s}\" ]").unwrap()
                    }
                },
                EdgeKind::SymbolTable => writeln!(
                    dot,
                    "{start}:s -> {end}:w [label=\"symbols\", color=blue, fontcolor=blue]"
                )
                .unwrap(),
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
    symbols: HashMap<String, usize>,
}

impl Block {
    fn new(block: &ssa::BlockId, block_data: &ssa::BasicBlockData) -> Self {
        let header = block_data.header.iter().map(Instruction::from).collect();
        let instructions = block_data.statements.iter().map(Instruction::new).collect();

        let mut symbols = HashMap::new();
        for (ident, id) in block_data.symbol_table.0.clone() {
            symbols.insert(ident.0, id.0);
        }

        //let terminator: Instruction = block_data.terminator.into();

        Block {
            id: block.0,
            header,
            instructions,
            symbols,
        }
    }

    fn write_record(&self, str: &mut String) -> Result<(), std::fmt::Error> {
        write!(str, "bb{} ", self.id)?;
        let mut label = format!("<b>BB{} | ", self.id);
        write!(label, "{{")?;
        let headers = self.header.iter().map(|it| it.to_string());
        let ins = self.instructions.iter().map(|it| it.to_string());

        let ins = headers.chain(ins).collect::<Vec<String>>().join(" | ");

        write!(label, "{ins}")?;

        write!(label, "}}")?;

        writeln!(str, "[shape=record, label=\"{label}\"];")
    }

    fn write_symbols(&self, str: &mut String) -> Result<Option<Edge>, std::fmt::Error> {
        write!(str, "sym{} ", self.id)?;
        let mut label = "".to_string(); //format!("<b>SYM{} | ", self.id);
        write!(label, "{{")?;

        let ins = self
            .symbols
            .iter()
            .map(|(symbol, id)| format!("{symbol}: {id}"))
            .collect::<Vec<String>>()
            .join(" | ");

        write!(label, "{ins}")?;

        write!(label, "}}")?;

        writeln!(str, "[shape=record, color=blue, label=\"{label}\"];")?;
        Ok(Some(Edge::new(
            format!("sym{0}", self.id),
            format!("bb{0}", self.id),
            EdgeKind::SymbolTable,
        )))
    }
}

enum Op {
    Add,
    AddI,
    Const,
}

enum Operand {
    InstructionReference(usize),
    Constant(i32),
}
impl Operand {
    fn from(id: InstructionId) -> Operand {
        Operand::InstructionReference(id.0)
    }
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
            ssa::InstructionKind::BasicOp(_op, l, r) => Instruction {
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![
                    Operand::InstructionReference(l.0),
                    Operand::InstructionReference(r.0),
                ],
            },
            InstructionKind::Read => Instruction {
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: Vec::new(),
            },
            InstructionKind::Write(ins) => Instruction {
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![Operand::from(ins)],
            },
        }
    }

    fn to_string(&self) -> String {
        //6: add (2) (2)
        let mut s = String::new();
        write!(s, "{}: {}", self.id, self.op).unwrap();
        for op in &self.operands {
            match op {
                Operand::InstructionReference(ins) => write!(s, " ({ins})").unwrap(),
                Operand::Constant(c) => write!(s, " #{c}").unwrap(),
            };
        }
        s
    }
}

impl From<InstructionKind> for String {
    fn from(val: InstructionKind) -> Self {
        match val {
            InstructionKind::Constant(_) => "Const".to_string(),
            InstructionKind::BasicOp(op, _, _) => match op {
                ssa::BasicOpKind::Add => "add".to_string(),
                ssa::BasicOpKind::Subtract => "sub".to_string(),
                ssa::BasicOpKind::Multiply => "mul".to_string(),
                ssa::BasicOpKind::Divide => "div".to_string(),
            },
            InstructionKind::Read => "read".to_string(),
            InstructionKind::Write(_) => "write".to_string(),
        }
    }
}

impl From<&ssa::HeaderInstruction> for Instruction {
    fn from(instruction: &ssa::HeaderInstruction) -> Self {
        match instruction.kind {
            ssa::HeaderStatementKind::Kill(ins) => Instruction {
                id: instruction.id.0,
                op: "kill".to_string(),
                operands: vec![Operand::InstructionReference(ins.0)],
            },
            ssa::HeaderStatementKind::Phi(l, r) => Instruction {
                id: instruction.id.0,
                op: "phi".to_string(),
                operands: vec![
                    Operand::InstructionReference(l.0),
                    Operand::InstructionReference(r.0),
                ],
            },
        }
    }
}

impl From<ssa::Terminator> for Instruction {
    fn from(_instruction: ssa::Terminator) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse, ssa::ssa::lower_program};

    #[test]
    fn test_if_dot() {
        pretty_env_logger::init();

        let program = r"
            let a <- call InputNum();
            let b <- 10;
            let b <- b+a;
            if a < b
            then 
                let b <- b + a;
            else 
                let b <- b + 5;
            fi
            let c <- b + 1;
            call OutputNum(c);

        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let cfg = lower_program(forest);
        let mut g = Graph::new(cfg);
        println!("{}", g.render());
    }

    #[test]
    fn test_dot_while() {
        pretty_env_logger::init();

        let program = r"
            let n <- call InputNum();
            let a <- 0;
            let b <- 1;
            let i <- 0;
            while i < n
            do 
                let sum <- a + b;
                let a <- b;
                let b <- sum;             
            od
            
            call OutputNum(b);

        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let cfg = lower_program(forest);
        let mut g = Graph::new(cfg);
        println!("{}", g.render());
    }

    #[test]
    fn test_io_fn_dot() {
        pretty_env_logger::init();

        let program = r"
            let a <- call InputNum();
            let a <- 0;
            let b <- 10;
            let c <- b+a;
            let c <- a + 1
            call OutputNum(c);
        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let cfg = lower_program(forest);
        let mut g = Graph::new(cfg);
        println!("{}", g.render());
    }

    #[test]
    fn test_dot_cse() {
        pretty_env_logger::init();

        let program = r"
            let a <- call InputNum();
            let b <- 3;
            let c <- 6; 
            let b <- a+ b *c - 4 + 7;
            let c <- a+ b *c - 4 + 7;
            let d <- a+ b *c - 4 + 7;
            let e <- a+ b *c - 4 + 7;
            let f <- a+ b *c - 4 + 7;
            let g <- a+ b *c - 4 + 7;
            call OutputNum(c);
        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let cfg = lower_program(forest);
        let mut g = Graph::new(cfg);
        println!("{}", g.render());
    }
}
