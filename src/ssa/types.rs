use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

//TODO remove a bunch of the `pub` declarations and provide controlled access methods instead
use crate::{parser::Expression, tokenizer::Ident};

// This design is shamelessly copied from `rustc`
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BasicOpKind {
    Add,
    Addi,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstructionKind {
    Constant(i32),
    BasicOp(BasicOpKind, InstructionId, InstructionId),
    Read,
    Write(InstructionId),
    Return(Option<InstructionId>),
    Call(Ident, Vec<InstructionId>),
    Load(InstructionId),
    // pointer, value
    Store(InstructionId, InstructionId)
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
    ConditionalBranch {
        condition: Comparison,
        target: BlockId,
        fallthrough: BlockId,
    },
}

#[derive(Debug, Clone)]
pub enum HeaderStatementKind {
    Kill(InstructionId),
    Phi(InstructionId, InstructionId),
    Param(Ident)
}

#[derive(Debug, Clone)]
pub struct SymbolTable(pub HashMap<Ident, InstructionId>);
impl SymbolTable {
    /**
     * Find all symbols that have changed between old_symbols and new_symbols.
     */
    pub(crate) fn changed_symbols(
        old_symbols: &SymbolTable,
        new_symbols: &SymbolTable,
    ) -> Vec<Mutation> {
        let mut symbols = Vec::new();

        for (ident, old) in &old_symbols.0 {
            if let Some(new) = new_symbols.0.get(ident) && new != old {
                symbols.push(Mutation::new(ident.clone(), *old, *new));
            }
        }
        symbols
    }

    pub(crate) fn set(&mut self, ident: Ident, id: InstructionId) -> Option<InstructionId> {
        self.0.insert(ident, id)
    }

    pub(crate) fn get_symbols(&self, instruction_id: InstructionId) -> Vec<Ident> {
        self.0
            .iter()
            .filter_map(|(ident, id)| {
                if *id == instruction_id {
                    Some(ident.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

// TODO make this not crap
#[derive(Debug, Clone)]
pub struct DominanceTable {
    basic_ops: HashMap<InstructionKind, InstructionId>,
}

impl DominanceTable {
    pub fn add_instruction(&mut self, instruction: &InstructionKind, id: InstructionId) {
        match instruction {
            InstructionKind::BasicOp(_, _, _) => {
                self.basic_ops.insert(instruction.clone(), id);
            }
            _ => {}
        }
    }

    pub fn get_dominating_instruction(
        &self,
        instruction: &InstructionKind,
    ) -> Option<InstructionId> {
        match instruction {
            InstructionKind::BasicOp(_, _, _) => self.basic_ops.get(instruction).copied(),
            _ => None,
        }
    }

    pub fn new() -> Self {
        DominanceTable {
            basic_ops: Default::default(),
        }
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
#[derive(Clone, Debug)]
pub(crate) struct Mutation {
    pub(crate) ident: Ident,
    pub(crate) old: InstructionId,
    pub(crate) new: InstructionId,
}
impl Mutation {
    fn new(ident: Ident, old: InstructionId, new: InstructionId) -> Mutation {
        Self { ident, old, new }
    }
}

fn replaced(
    ins: InstructionId,
    replacements: &HashMap<InstructionId, InstructionId>,
) -> InstructionId {
    replacements.get(&ins).copied().unwrap_or(ins)
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

    // pub(crate) fn replace(&mut self, replacements: &HashMap<InstructionId, InstructionId>) {
    //     self.header = self
    //         .header
    //         .iter()
    //         .map(|instruction| match instruction.kind {
    //             HeaderStatementKind::Kill(_) => todo!(),
    //             HeaderStatementKind::Phi(l, r) => HeaderInstruction {
    //                 kind: HeaderStatementKind::Phi(
    //                     replaced(l, replacements),
    //                     replaced(r, replacements),
    //                 ),
    //                 id: instruction.id,
    //             },
    //         })
    //         .collect();

    //     self.statements = self
    //         .statements
    //         .iter()
    //         .map(|instruction| match &instruction.kind {
    //             InstructionKind::Constant(_) => instruction.clone(),
    //             InstructionKind::BasicOp(op, l, r) => Instruction {
    //                 kind: InstructionKind::BasicOp(
    //                     *op,
    //                     replaced(*l, replacements),
    //                     replaced(*r, replacements),
    //                 ),
    //                 dominating_instruction: instruction.dominating_instruction,
    //                 id: instruction.id,
    //             },
    //             InstructionKind::Read => instruction.clone(),
    //             InstructionKind::Write(_) => instruction.clone(),
    //         })
    //         .collect();
    //     if let Some(Terminator::ConditionalBranch {
    //         ref mut condition,
    //         target: _,
    //         fallthrough: _,
    //     }) = &mut self.terminator
    //     {
    //         (*condition).value = replaced(condition.value, replacements);
    //     };
    // }

    pub(crate) fn update_phi(&mut self, instruction_id: InstructionId, new_target: InstructionId) {
        let ins = self
            .header
            .iter_mut()
            .find(|it| it.id == instruction_id)
            .unwrap();
        match ins.kind {
            HeaderStatementKind::Kill(_) => panic!(),
            HeaderStatementKind::Phi(_, ref mut new) => *new = new_target,
            HeaderStatementKind::Param(_) => panic!(),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.header.is_empty() && self.statements.is_empty()
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct InstructionId(pub usize);
