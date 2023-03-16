use std::{collections::HashMap, fmt::Write};

use crate::parser::Function;
use crate::{lower_program, parse, SourceFile};

use super::lower_function;
use super::types::ComparisonKind;

use super::cfg::ControlFlowGraph;
use super::types::{
    BasicBlockData, BlockId, Comparison, InstructionId, InstructionKind, Terminator,
};

#[derive(Debug)]
enum EdgeKind {
    Goto,
    Conditional(String),
    SymbolTable,
    Dominance,
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
}
#[derive(Debug)]
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

#[derive(Debug)]
pub struct Graph {
    functions: Vec<FunctionGraph>,
}
impl Graph {
    fn new(functions: Vec<FunctionGraph>) -> Self {
        Graph { functions }
    }

    //TODO make this not mutable
    fn render(&mut self) -> String {
        let mut dot = String::new();
        writeln!(dot, "digraph {{").unwrap();

        for func in &self.functions {
            //func.write_symbols(&mut dot);
            func.render(&mut dot);
        }
        writeln!(dot, "}}").unwrap();
        dot
    }
}

#[derive(Debug)]
pub struct FunctionGraph {
    function: Function,
    blocks: Vec<Block>,
    edges: Vec<Edge>,
}

impl FunctionGraph {
    pub fn new(function: Function, cfg: ControlFlowGraph) -> Self {
        let blocks = cfg.blocks().collect::<Vec<(BlockId, BasicBlockData)>>();
        let edges = collect_edges(&blocks);

        let blocks = blocks
            .iter()
            .map(|(id, bb_data)| Block::new(id, bb_data, function.name.0.clone()))
            .collect();
        FunctionGraph {
            function,
            blocks,
            edges,
        }
    }

    // pub fn write_symbols(&mut self, dot: &mut String) {
    //     for block in &self.blocks {
    //         block.write_record(dot).unwrap();
    //         let edge = block.write_symbols(dot).unwrap().unwrap();
    //         self.edges.push(edge);
    //     }
    // }

    pub fn render(&self, dot: &mut String) {
        for block in &self.blocks {
            //block.write_record(dot).unwrap();
            block.write_table(dot).unwrap();
        }

        let prefix = &self.function.name.0;

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
                    Direction::Up => writeln!(dot, "{prefix}_{start}:s -> {prefix}_{end}:e").unwrap(),
                    Direction::Down => writeln!(dot, "{prefix}_{start}:s -> {prefix}_{end}:n").unwrap(),
                },

                EdgeKind::Conditional(s) => match direction {
                    Direction::Up => writeln!(dot, "{prefix}_{start}:s -> {prefix}_{end}:e [label=\"{s}\" ]").unwrap(),
                    Direction::Down => writeln!(dot, "{prefix}_{start}:s -> {prefix}_{end}:n [label=\"{s}\" ]").unwrap(),
                },
                EdgeKind::SymbolTable => writeln!(
                    dot,
                    "{start}:s -> {prefix}_{end}:w [label=\"symbols\", color=blue, arrowsize=0.6, fontcolor=blue, fontsize=8, style=dotted, arrowhead=vee]"
                )
                .unwrap(),
                EdgeKind::Dominance => writeln!(
                    dot,
                    "{prefix}_{start}:ne -> {prefix}_{end}:se [label=\"dom\", color=red, arrowsize=0.6, fontcolor=red, fontsize=8, style=dotted, arrowhead=vee]"
                )
                .unwrap(),
            };
        }

        //write!(dot, "}}").unwrap();
    }
}

fn collect_edges(blocks: &Vec<(BlockId, BasicBlockData)>) -> Vec<Edge> {
    let mut edges = Vec::new();
    for (block, block_data) in blocks {
        if let Some(x) = block_data.dominating_block {
            edges.push(Edge::dominance(*block, x));
        };

        if let Some(x) = &block_data.terminator {
            match x {
                Terminator::Goto(target) => edges.push(Edge::bb(*block, *target, EdgeKind::Goto)),
                Terminator::ConditionalBranch {
                    condition,
                    target,
                    fallthrough,
                } => {
                    // TODO include condition details in graph
                    edges.push(Edge::bb(
                        *block,
                        *target,
                        EdgeKind::Conditional("true".to_string()),
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
    edges
}

#[derive(Debug)]
struct Block {
    id: usize,
    header: Vec<Instruction>,
    instructions: Vec<Instruction>,
    symbols: HashMap<String, usize>,
    prefix: String,
    terminator: String,
}

// impl Into<String> for Comparison {
//     fn into(self) -> String {
//         let id = self.value;
//         let rhs = match self.kind {
//             ComparisonKind::LtZero => "<0",
//             ComparisonKind::LteZero => "<=0",
//             ComparisonKind::GtZero => ">0",
//             ComparisonKind::GteZero => ">=0",
//             ComparisonKind::EqZero => "==0",
//             ComparisonKind::NeZero => "!=0",
//         };
//         format!("({id}){rhs}")
//     }
// }

impl Block {
    fn new(block: &BlockId, block_data: &BasicBlockData, prefix: String) -> Self {
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

        let terminator: String = match &block_data.terminator {
            Some(Terminator::Goto(target)) => "goto".to_owned(),
            Some(Terminator::ConditionalBranch {
                condition,
                target,
                fallthrough,
            }) => format!("if {}", html_escape::encode_text(&condition.to_string())),
            None => "return".to_owned(),
        };

        Block {
            id: block.0,
            header,
            instructions,
            symbols,
            prefix,
            terminator,
        }
    }

    fn write_table(&self, str: &mut String) -> Result<(), std::fmt::Error> {
        let prefix = &self.prefix;

        let short_prefix = &self.prefix.chars().take(4).collect::<String>();

        let id = self.id;
        let table_params = r#"BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4""#;
        let basic_block = format!("<TR><TD ROWSPAN=\"100\">BB{id}<BR/>{short_prefix}</TD></TR>");
        let mut label = format!(
            "<TABLE {table_params}>
                {basic_block}
            "
        );
        let block_name = format!("{prefix}_bb{id}");

        let ins = self
            .header
            .iter()
            .chain(self.instructions.iter())
            .map(|it| it.to_row())
            .map(|(symbols, instruction, dominance)| {
                format!("<TR><TD>{symbols}</TD><TD>{instruction}</TD><TD color=\"red\">{dominance}</TD></TR>")
            })
            .collect::<Vec<String>>()
            .join("");

        write!(label, "{ins}")?;
        write!(
            label,
            "<TR><TD color=\"blue\" colspan=\"4\">{}</TD></TR>",
            self.terminator
        )?;
        write!(label, "</TABLE>")?;
        writeln!(str, "{block_name} [shape=plain, label=<{label}>];")
    }

    fn write_record(&self, str: &mut String) -> Result<(), std::fmt::Error> {
        let prefix = &self.prefix;

        let short_prefix = &self.prefix.chars().take(4).collect::<String>();

        write!(str, "{prefix}_bb{} ", self.id)?;
        let mut label = format!("<b>{}_BB{} | ", short_prefix, self.id);
        write!(label, "{{")?;
        let headers = self.header.iter();
        let ins = self.instructions.iter();

        let ins = headers
            .chain(ins)
            .map(|it| it.to_string())
            .collect::<Vec<String>>()
            .join(" | ");

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

#[derive(Debug)]
enum Operand {
    InstructionReference(usize),
    Ident(String),
    Constant(i32),
}
impl Operand {
    fn from(id: InstructionId) -> Operand {
        Operand::InstructionReference(id.0)
    }
}
#[derive(Debug)]
struct Instruction {
    symbols: Vec<String>,
    id: usize,
    op: String,
    operands: Vec<Operand>,
    dominated: Option<String>,
}

impl Instruction {
    fn from_intruction(
        instruction: &super::types::Instruction,
        block_data: &BasicBlockData,
    ) -> Self {
        match instruction.kind.clone() {
            super::types::InstructionKind::Constant(c) => Instruction {
                symbols: get_symbols(block_data, instruction.id),
                id: instruction.id.0,
                op: "Const".to_string(),
                operands: vec![Operand::Constant(c)],
                dominated: instruction
                    .dominating_instruction
                    .map(|it| it.0.to_string()),
            },
            super::types::InstructionKind::BasicOp(_op, l, r) => Instruction {
                symbols: get_symbols(block_data, instruction.id),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![
                    Operand::InstructionReference(l.0),
                    Operand::InstructionReference(r.0),
                ],
                dominated: instruction
                    .dominating_instruction
                    .map(|it| it.0.to_string()),
            },
            InstructionKind::Read => Instruction {
                symbols: get_symbols(block_data, instruction.id),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: Vec::new(),
                dominated: instruction
                    .dominating_instruction
                    .map(|it| it.0.to_string()),
            },
            InstructionKind::Write(ins) => Instruction {
                symbols: Vec::new(),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![Operand::from(ins)],
                dominated: instruction
                    .dominating_instruction
                    .map(|it| it.0.to_string()),
            },
            InstructionKind::Return(operand) => Instruction {
                symbols: Vec::new(),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: if operand.is_some() {
                    vec![Operand::InstructionReference(operand.unwrap().0)]
                } else {
                    vec![]
                },
                dominated: None,
            },
            InstructionKind::Call(ident, arguments) => Instruction {
                symbols: Vec::new(),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: arguments
                    .iter()
                    .map(|it| Operand::InstructionReference(it.0))
                    .collect(),
                dominated: None,
            },
            InstructionKind::Load(pointer) => Instruction {
                symbols: Vec::new(),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![Operand::InstructionReference(pointer.0)],
                dominated: None,
            },
            InstructionKind::Store(pointer, value) => Instruction {
                symbols: Vec::new(),
                id: instruction.id.0,
                op: instruction.kind.clone().into(),
                operands: vec![
                    Operand::InstructionReference(pointer.0),
                    Operand::InstructionReference(value.0),
                ],
                dominated: None,
            },
        }
    }

    fn to_string(&self) -> String {
        //6: add (2) (2)
        let mut s = String::new();
        if !self.symbols.is_empty() {
            write!(s, "({}) ", self.symbols.join(",")).unwrap();
        }
        write!(s, "{}: {}", self.id, self.op).unwrap();

        for op in &self.operands {
            match op {
                Operand::InstructionReference(ins) => write!(s, " ({ins})").unwrap(),
                Operand::Constant(c) => write!(s, " #{c}").unwrap(),
                Operand::Ident(str) => write!(s, " {str}").unwrap(),
            };
        }
        if let Some(id) = &self.dominated {
            write!(s, " (D:{})", id).unwrap();
        }
        s
    }

    fn to_row(&self) -> (String, String, String) {
        //6: add (2) (2)
        let mut symbols = String::new();
        let mut instruction = String::new();
        let mut dominance = String::new();
        if !self.symbols.is_empty() {
            write!(symbols, "{}", self.symbols.join(",")).unwrap();
        }
        write!(instruction, "{}: {}", self.id, self.op).unwrap();

        for op in &self.operands {
            match op {
                Operand::InstructionReference(ins) => write!(instruction, " ({ins})").unwrap(),
                Operand::Constant(c) => write!(instruction, " #{c}").unwrap(),
                Operand::Ident(str) => write!(instruction, " {str}").unwrap(),
            };
        }
        if let Some(id) = &self.dominated {
            write!(dominance, " D:{}", id).unwrap();
        }
        (symbols, instruction, dominance)
    }

    fn from_header(
        instruction: &super::types::HeaderInstruction,
        block_data: &BasicBlockData,
    ) -> Self {
        match &instruction.kind {
            super::types::HeaderStatementKind::Kill(ins) => Instruction {
                symbols: Vec::new(),
                id: instruction.id.0,
                op: "kill".to_string(),
                operands: vec![Operand::InstructionReference(ins.0)],
                dominated: None,
            },
            super::types::HeaderStatementKind::Phi(l, r) => Instruction {
                symbols: get_symbols(block_data, instruction.id),
                id: instruction.id.0,
                op: "phi".to_string(),
                operands: vec![
                    Operand::InstructionReference(l.0),
                    Operand::InstructionReference(r.0),
                ],
                dominated: None,
            },
            super::types::HeaderStatementKind::Param(ident) => Instruction {
                symbols: get_symbols(block_data, instruction.id),
                id: instruction.id.0,
                op: "param".to_string(),
                operands: vec![Operand::Ident(ident.0.clone())],
                dominated: None,
            },
        }
    }
}

fn get_symbols(block_data: &BasicBlockData, instruction: InstructionId) -> Vec<String> {
    block_data
        .symbol_table
        .get_symbols(instruction)
        .iter()
        .map(|it| it.0.clone())
        .collect()
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
                super::types::BasicOpKind::Addi => "addi".to_owned(),
            },
            InstructionKind::Read => "read".to_string(),
            InstructionKind::Write(_) => "write".to_string(),
            InstructionKind::Return(_) => "return".to_string(),
            InstructionKind::Call(ident, _) => format!("call_{}", ident.0),
            InstructionKind::Load(_) => "load".to_owned(),
            InstructionKind::Store(_, _) => "store".to_owned(),
        }
    }
}

impl From<super::types::Terminator> for Instruction {
    fn from(_instruction: super::types::Terminator) -> Self {
        todo!()
    }
}

pub fn render_program(code: &str) -> String {
    let file = SourceFile::new(code);
    let forest = parse(file);
    //println!("{forest:#?}");

    let functions = forest
        .functions
        .iter()
        .map(|function| {
            let cfg = lower_function(&function);
            FunctionGraph::new(function.clone(), cfg)
        })
        .collect();

    let mut graph = Graph::new(functions);
    // println!("{:#?}", graph);
    graph.render()
}
