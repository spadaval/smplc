use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

use log::warn;

//TODO remove a bunch of the `pub` declarations and provide controlled access methods instead
use crate::{parser::Variable, tokenizer::Ident};

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
    pub dominator: Option<InstructionId>,
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
// pointer, value
pub struct Store {
    pub base: InstructionId,
    pub pointer: InstructionId,
    pub value: InstructionId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pointer, value
pub struct Load {
    pub base: InstructionId,
    pub pointer: InstructionId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstructionKind {
    Constant(i32),
    BasicOp(BasicOpKind, InstructionId, InstructionId),
    Read,
    Write(InstructionId),
    Return(Option<InstructionId>),
    Call(Ident, Vec<InstructionId>),
    Load(Load),
    Store(Store),
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

#[derive(Debug, Clone, EnumIntoGetters, EnumAsGetters, EnumIsA)]
pub enum Terminator {
    Goto(BlockId),
    ConditionalBranch {
        condition: Comparison,
        target: BlockId,
        fallthrough: BlockId,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum Linkage {
    Global,
    Extern,
    Local,
}

#[derive(Debug, Clone)]
pub enum HeaderStatementKind {
    Kill(InstructionId),
    Phi(InstructionId, InstructionId),
    Param(Variable),
    Variable(Linkage, Variable),
}

#[derive(Debug, Clone)]
pub struct SymbolTable(pub HashMap<Variable, InstructionId>);
impl SymbolTable {
    /**
     * Find all symbols that have changed between old_symbols and new_symbols.
     */
    pub(crate) fn changed_symbols(
        old_symbols: &SymbolTable,
        new_symbols: &SymbolTable,
    ) -> Vec<Mutation> {
        let mut symbols = Vec::new();

        for (var, old) in &old_symbols.0 {
            if let Some(new) = new_symbols.0.get(var) && new != old {
                symbols.push(Mutation::new(var.ident.clone(), *old, *new));
            }
        }
        symbols
    }

    pub fn get(&self, ident: &Ident) -> Option<InstructionId> {
        self.0
            .iter()
            .find(|it| it.0.ident == *ident)
            .map(|it| *it.1)
    }

    pub(crate) fn update(&mut self, ident: &Ident, id: InstructionId) {
        let existing_def = self.0.iter_mut().find(|it| it.0.ident == *ident);
        match existing_def {
            Some(def) => *def.1 = id,
            None => panic!("Tried to update an undefined variable {:?}", ident),
        };
    }

    pub(crate) fn update_or_init(&mut self, ident: &Ident, id: InstructionId) {
        let existing_def = self.0.iter_mut().find(|it| it.0.ident == *ident);
        match existing_def {
            Some(def) => *def.1 = id,
            None => {self.0.insert(Variable::i32(ident.clone()) , id);},
        };
    }

    pub(crate) fn set(&mut self, variable: Variable, id: InstructionId) {
        self.0.insert(variable, id);
    }

    pub(crate) fn get_symbols(&self, instruction_id: InstructionId) -> Vec<Variable> {
        self.0
            .iter()
            .filter_map(|(var, id)| {
                if *id == instruction_id {
                    Some(var.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    pub(crate) fn copy_from(&mut self, symbols: &SymbolTable) {
        for (ident, id) in &symbols.0 {
            if !self.0.contains_key(ident) {
                self.0.insert(ident.clone(), *id);
            }
        }
    }
}

// TODO make this not crap
#[derive(Debug, Clone)]
pub struct DominanceTable {
    basic_ops: HashMap<Discriminant<BasicOpKind>, InstructionId>,
    load_store: Option<InstructionId>,
}

impl DominanceTable {
    pub fn add_instruction(&mut self, instruction: &InstructionKind, id: InstructionId) {
        match instruction {
            InstructionKind::BasicOp(op, _, _) => {
                self.basic_ops.insert(mem::discriminant(op), id);
            }
            InstructionKind::Load(_) | InstructionKind::Store(_) => self.load_store = Some(id),
            _ => {}
        }
    }

    pub fn get_dominating_instruction(
        &self,
        instruction: &InstructionKind,
    ) -> Option<InstructionId> {
        match instruction {
            InstructionKind::BasicOp(op, _, _) => {
                self.basic_ops.get(&mem::discriminant(op)).copied()
            }
            InstructionKind::Load(_) | InstructionKind::Store(_) => self.load_store,
            _ => None,
        }
    }

    pub fn get_dominating_loadstore(&self, _instruction: &InstructionKind) -> Option<InstructionId> {
        self.load_store
    }

    pub fn new() -> Self {
        DominanceTable {
            basic_ops: Default::default(),
            load_store: None,
        }
    }

    pub(crate) fn copy_from(&mut self, table: &DominanceTable) {
        for (key, id) in &table.basic_ops {
            if !self.basic_ops.contains_key(key) {
                self.basic_ops.insert(*key, *id);
            }
            if self.load_store.is_none() {
                self.load_store = table.load_store;
            }
        }
    }

    pub(crate) fn register_kill(&mut self, kill: InstructionId) {
        self.load_store = Some(kill);
    }

    pub(crate) fn get_load_store(&self) -> Option<InstructionId> {
        self.load_store
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

    pub(crate) fn update_phi(&mut self, instruction_id: InstructionId, new_target: InstructionId) {
        let ins = self
            .header
            .iter_mut()
            .find(|it| it.id == instruction_id)
            .unwrap();
        match ins.kind {
            HeaderStatementKind::Phi(_, ref mut new) => *new = new_target,
            _ => panic!(),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.header.is_empty() && self.statements.is_empty()
    }

    pub(crate) fn reset(&mut self) {
        *self = BasicBlockData::new();
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct InstructionId(pub usize);
