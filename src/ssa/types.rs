use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};



//TODO remove a bunch of the `pub` declarations and provide controlled access methods instead
use crate::{
    parser::{Expression},
    tokenizer::Ident,
};

// This design is shamelessly copied from `rustc`
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub dominating_instruction: Option<InstructionId>,
    pub id: InstructionId,
}
#[derive(Debug, Clone)]
pub struct HeaderInstruction {
    pub kind: HeaderStatementKind,
    pub id: InstructionId,
}
#[derive(Debug, Clone)]
pub enum BasicOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    Constant(i32),
    BasicOp(BasicOpKind, InstructionId, InstructionId),
    Read,
    Write(InstructionId),
}

#[derive(Debug, Clone)]
pub struct Comparison {
    pub kind: ComparisonKind,
    pub value: InstructionId,
}

// For some reason DLX has all of these instructions. Might as well use them.
// They are easy enough to emulate.
#[derive(Debug, Clone)]
pub enum ComparisonKind {
    LtZero,
    LteZero,
    GtZero,
    GteZero,
    EqZero,
    NeZero,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(BlockId),
    ConditionalBranch { condition: Comparison, target: BlockId, fallthrough: BlockId },
}

#[derive(Debug, Clone)]
pub enum HeaderStatementKind {
    Kill(InstructionId),
    Phi(InstructionId, InstructionId),
}

#[derive(Debug, Clone)]
pub struct SymbolTable(pub HashMap<Ident, InstructionId>);

#[derive(Debug, PartialEq, Clone)]
pub struct DominanceTable {
    expressions: HashMap<Discriminant<Expression>, InstructionId>,
}

impl DominanceTable {
    pub fn add_expression(&mut self, expr: &Expression, id: InstructionId) {
        self.expressions.insert(mem::discriminant(expr), id);
    }

    pub fn get_expression(&self, expr: &Expression) -> Option<InstructionId> {
        self.expressions.get(&mem::discriminant(expr)).copied()
    }

    pub fn new() -> Self {
        DominanceTable { expressions: Default::default() }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlockData {
    pub header: Vec<HeaderInstruction>,
    pub statements: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub symbol_table: SymbolTable,
    pub dominance_table: DominanceTable,
    pub dominating_block: Option<BlockId>,
}

impl BasicBlockData {
    pub fn new() -> Self {
        Self {
            header: Vec::new(),
            statements: Vec::new(),
            terminator: None,
            symbol_table: SymbolTable(Default::default()),
            dominance_table: DominanceTable::new(),
            dominating_block: None,
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct InstructionId(pub usize);
