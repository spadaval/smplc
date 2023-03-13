use std::{collections::HashMap, fmt::Write};

use crate::{lower_program, parse, SourceFile};

use super::types::ComparisonKind;

use super::cfg::ControlFlowGraph;
use super::types::{
    BasicBlockData, BlockId, Comparison, InstructionId, InstructionKind, Terminator,
};

enum EdgeKind {
    Goto,
    Conditional(String),
    SymbolTable,
    Dominance,
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

impl Comparison {
    fn to_string(&self) -> String {
        let comp = match self.kind {
            ComparisonKind::LtZero => "<",
            ComparisonKind::LteZero => "<=",
            ComparisonKind::GtZero => ">",
            ComparisonKind::GteZero => ">=",
            ComparisonKind::EqZero => "=",
            ComparisonKind::NeZero => "!=",
        };
        format!("({}) {} 0", self.value.0, comp)
    }
}

impl Edge {
    fn bb(start: BlockId, end: BlockId, kind: EdgeKind) -> Self {
        Edge {
            start: format!("bb{}", start.0),
            end: format!("bb{}", end.0),
            kind,
            direction: if end.0 > start.0 {
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

    fn dominance(start: BlockId, end: BlockId) -> Edge {
        Edge {
            start: format!("bb{}", start.0),
            end: format!("bb{}", end.0),
            kind: EdgeKind::Dominance,
            direction: Direction::Up,
        }
    }
}
pub struct Graph {
    blocks: Vec<Block>,
    edges: Vec<Edge>,
}

impl Graph {
    pub fn new(cfg: ControlFlowGraph) -> Self {
        let blocks = cfg.blocks().collect::<Vec<(BlockId, BasicBlockData)>>();
        let mut edges = Vec::new();
        for (block, block_data) in &blocks {
            if let Some(x) = block_data.dominating_block {
                edges.push(Edge::dominance(*block, x));
            };

            if let Some(x) = &block_data.terminator {
                match x {
                    Terminator::Goto(target) => {
                        edges.push(Edge::bb(*block, *target, EdgeKind::Goto))
                    }
                    Terminator::ConditionalBranch {
                        condition,
                        target,
                        fallthrough,
                    } => {
                        // TODO include condition details in graph
                        edges.push(Edge::bb(
                            *block,
                            *target,
                            EdgeKind::Conditional(condition.to_string()),
                        ));
                        edges.push(Edge::bb(
                            *block,
                            *fallthrough,
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
                    Direction::Up => writeln!(dot, "{start}:s -> {end}:e [label=\"goto\" ]").unwrap(),
                    Direction::Down => writeln!(dot, "{start}:s -> {end}:n [label=\"goto\" ]").unwrap(),
                },

                EdgeKind::Conditional(s) => match direction {
                    Direction::Up => writeln!(dot, "{start}:s -> {end}:e [label=\"{s}\" ]").unwrap(),
                    Direction::Down => writeln!(dot, "{start}:s -> {end}:n [label=\"{s}\" ]").unwrap(),
                },
                EdgeKind::SymbolTable => writeln!(
                    dot,
                    "{start}:s -> {end}:w [label=\"symbols\", color=blue, arrowsize=0.6, fontcolor=blue, fontsize=8, style=dotted, arrowhead=vee]"
                )
                .unwrap(),
                EdgeKind::Dominance => writeln!(
                    dot,
                    "{start}:ne -> {end}:s [label=\"dom\", color=red, arrowsize=0.6, fontcolor=red, fontsize=8, style=dotted, arrowhead=vee]"
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
    fn new(block: &BlockId, block_data: &BasicBlockData) -> Self {
        let header = block_data
            .header
            .iter()
            .map(|header| Instruction::from_header(header, block_data))
            .collect();
        let instructions = block_data
            .statements
            .iter()
            .map(|it| Instruction::from_intruction(it, block_data))
            .collect();

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

        writeln!(
            str,
            "[shape=record, fontsize=8, width=0.1, color=blue, label=\"{label}\"];"
        )?;
        Ok(Some(Edge::new(
            format!("sym{0}", self.id),
            format!("bb{0}", self.id),
            EdgeKind::SymbolTable,
        )))
    }
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
    symbol: Option<String>,
    id: usize,
    op: String,
    operands: Vec<Operand>,
}

impl Instruction {
    fn from_intruction(
        instruction: &super::types::Instruction,
        block_data: &BasicBlockData,
    ) -> Self {
        match instruction.kind.clone() {
            super::types::InstructionKind::Constant(c) => Instruction {
                symbol: get_symbol(block_data, instruction.id),
                id: instruction.id.0,
                op: "Const".to_string(),
                operands: vec![Operand::Constant(c)],
            },
            super::types::InstructionKind::BasicOp(_op, l, r) => Instruction {
                symbol: get_symbol(block_data, instruction.id),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![
                    Operand::InstructionReference(l.0),
                    Operand::InstructionReference(r.0),
                ],
            },
            InstructionKind::Read => Instruction {
                symbol: get_symbol(block_data, instruction.id),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: Vec::new(),
            },
            InstructionKind::Write(ins) => Instruction {
                symbol: None,
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![Operand::from(ins)],
            },
        }
    }

    fn to_string(&self) -> String {
        //6: add (2) (2)
        let mut s = String::new();
        match &self.symbol {
            Some(symbol) => write!(s, "({}) {}: {}", symbol, self.id, self.op).unwrap(),
            None => write!(s, "{}: {}", self.id, self.op).unwrap(),
        }

        for op in &self.operands {
            match op {
                Operand::InstructionReference(ins) => write!(s, " ({ins})").unwrap(),
                Operand::Constant(c) => write!(s, " #{c}").unwrap(),
            };
        }
        s
    }

    fn from_header(
        instruction: &super::types::HeaderInstruction,
        block_data: &BasicBlockData,
    ) -> Self {
        match instruction.kind {
            super::types::HeaderStatementKind::Kill(ins) => Instruction {
                symbol: None,
                id: instruction.id.0,
                op: "kill".to_string(),
                operands: vec![Operand::InstructionReference(ins.0)],
            },
            super::types::HeaderStatementKind::Phi(l, r) => Instruction {
                symbol: get_symbol(block_data, instruction.id),
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

fn get_symbol(block_data: &BasicBlockData, instruction: InstructionId) -> Option<String> {
    block_data
        .symbol_table
        .get_symbol(instruction)
        .map(|it| it.0)
}

impl From<InstructionKind> for String {
    fn from(val: InstructionKind) -> Self {
        match val {
            InstructionKind::Constant(_) => "Const".to_string(),
            InstructionKind::BasicOp(op, _, _) => match op {
                super::types::BasicOpKind::Add => "add".to_string(),
                super::types::BasicOpKind::Subtract => "sub".to_string(),
                super::types::BasicOpKind::Multiply => "mul".to_string(),
                super::types::BasicOpKind::Divide => "div".to_string(),
            },
            InstructionKind::Read => "read".to_string(),
            InstructionKind::Write(_) => "write".to_string(),
        }
    }
}

impl From<super::types::Terminator> for Instruction {
    fn from(_instruction: super::types::Terminator) -> Self {
        todo!()
    }
}

pub fn render_program(code: String) -> String {
    let file = SourceFile::new(&code);
    let forest = parse(file);
    //println!("{forest:#?}");
    let cfg = lower_program(forest);
    let mut g = Graph::new(cfg);
    g.render()
}

#[cfg(test)]
mod tests {
    use crate::{lower_program, parse};

    use super::*;

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
    fn test_dot_if_while() {
        pretty_env_logger::init();

        let program = r"
            let n <- call InputNum();
            let a <- 0;
            let b <- 1;
            let i <- 0;
            while i < n
            do 
                let i <- i + 1;
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
