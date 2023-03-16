use log::{debug, info, warn};

use crate::{parser::Expression, tokenizer::Ident};

use super::types::*;

#[derive(Debug)]
pub struct ControlFlowGraph {
    basic_blocks: Vec<BasicBlockData>,
    instruction_count: usize,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        let mut cfg = ControlFlowGraph {
            basic_blocks: Vec::new(),
            instruction_count: 0,
        };
        let const_block_id = cfg.new_start_block();
        cfg.get_constant(0);

        let start_block_id = cfg.new_block(const_block_id);

        cfg.goto(const_block_id, start_block_id);

        cfg
    }

    pub fn start_block(&self) -> BlockId {
        assert!(self.basic_blocks.len() >= 2);
        BlockId(1)
    }

    pub(crate) fn get_block(&self, block_id: BlockId) -> &BasicBlockData {
        &self.basic_blocks[block_id.0]
    }

    pub fn blocks(&self) -> impl Iterator<Item = (BlockId, BasicBlockData)> {
        self.basic_blocks
            .clone()
            .into_iter()
            .enumerate()
            .map(|(id, data)| (BlockId(id), data))
    }

    //FIXME make this private
    pub(crate) fn block_data_mut(&mut self, blk: BlockId) -> &mut BasicBlockData {
        &mut self.basic_blocks[blk.0]
    }

    pub fn new_block(&mut self, dominating_block: BlockId) -> BlockId {
        let dominating_block_data = self.get_block(dominating_block);

        let mut data = BasicBlockData::new();
        data.symbol_table = dominating_block_data.symbol_table.clone();
        data.dominance_table = dominating_block_data.dominance_table.clone();
        data.dominating_block = Some(dominating_block);

        self.basic_blocks.push(data);
        BlockId(self.basic_blocks.len() - 1)
    }

    fn new_start_block(&mut self) -> BlockId {
        let data = BasicBlockData::new();
        self.basic_blocks.push(data);
        BlockId(self.basic_blocks.len() - 1)
    }

    pub fn get_symbols(&self, block: BlockId) -> &SymbolTable {
        return &self.get_block(block).symbol_table;
    }

    // fn add_block(&mut self, block: BasicBlockData) -> BlockId {
    //     let new_block_id = self.basic_blocks.len();
    //     self.basic_blocks.push(block);
    //     BlockId(new_block_id)
    // }

    fn zero_value(&mut self) -> InstructionId {
        self.get_constant(0)
    }

    pub fn update_symbol(&mut self, block: BlockId, ident: Ident, val: InstructionId) {
        self.basic_blocks[block.0].symbol_table.0.insert(ident, val);
    }

    fn issue_instruction_id(&mut self) -> InstructionId {
        let new_id = InstructionId(self.instruction_count);
        self.instruction_count += 1;
        new_id
    }

    pub fn add_instruction(
        &mut self,
        block: BlockId,
        instruction: InstructionKind,
    ) -> InstructionId {
        let dominating_instruction = self.find_dominating_instruction(block, &instruction);

        if let Some(ins) = dominating_instruction {
            return ins;
        }

        let new_id = self.issue_instruction_id();
        let instruction = Instruction {
            kind: instruction,
            id: new_id,
            dominating_instruction,
        };

        let data = self.block_data_mut(block);
        data.dominance_table
            .add_instruction(&instruction.kind, instruction.id);
        data.statements.push(instruction);
        new_id
    }

    fn find_dominating_instruction(
        &mut self,
        block: BlockId,
        instruction: &InstructionKind,
    ) -> Option<InstructionId> {
        let dominating_instruction = self
            .get_block(block)
            .dominance_table
            .get_dominating_instruction(instruction)
            .and_then(|it| self.get_instruction(it));

        info!("Tried lookup for {instruction:?}");
        warn!("Got {dominating_instruction:?}");
        info!("table: {:?}", self.get_block(block).dominance_table);
        let mut curr_dom_instruction = dominating_instruction;

        while let Some(ins) = curr_dom_instruction {
            if ins.kind == *instruction {
                return Some(ins.id);
            } else {
                curr_dom_instruction = self.get_instruction(ins.id);
                assert!(curr_dom_instruction != Some(ins));
            }
        }

        None
    }

    pub fn add_header_statement(
        &mut self,
        block: BlockId,
        instruction: HeaderStatementKind,
    ) -> InstructionId {
        let new_id = self.issue_instruction_id();
        let data = self.block_data_mut(block);
        let instruction = HeaderInstruction {
            kind: instruction,
            id: new_id,
        };

        data.header.push(instruction);
        new_id
    }

    //TODO cache this maybe?
    fn predeccessors(&self, block: BlockId) -> Vec<BlockId> {
        self.blocks()
            .filter_map(|(id, data)| match data.terminator {
                Some(Terminator::ConditionalBranch {
                    condition: _,
                    target,
                    fallthrough,
                }) if target == block || fallthrough == block => Some(id),
                Some(Terminator::Goto(target)) if target == block => Some(id),
                _ => None,
            })
            .collect()
    }

    // Semantics: If the symbol exists in the symbol table, that's what we use.
    // otherwise, we Phi all the references in incoming blocks.
    // If no reference exists in one or more blocks, they are initialized to zero_value.
    // If all parents don't define the variable, we simply return the zero_value.
    // this only looks one-level above, so dominance shouldn't be a problem
    fn lookup_symbol(&mut self, block: BlockId, ident: &Ident) -> InstructionId {
        let current_block = self.get_block(block);
        if let Some(x) = current_block.symbol_table.0.get(ident) {
            return *x;
        }

        let references: Vec<(BlockId, Option<InstructionId>)> = self
            .predeccessors(block)
            .iter()
            .map(|block_id| {
                let data = self.get_block(*block_id);
                (*block_id, data.symbol_table.0.get(ident).copied())
            })
            .collect();

        if references.iter().all(|it| it.1.is_none()) {
            return self.zero_value();
        }

        assert!(references.len() == 2);

        let phi = HeaderStatementKind::Phi(
            references[0].1.unwrap_or(self.zero_value()),
            references[1].1.unwrap_or(self.zero_value()),
        );
        self.add_header_statement(block, phi)
    }

    // TODO make this return an error if a symbol can't be resolved correctly (for while loops)
    pub fn resolve_symbol(&mut self, block: BlockId, ident: &Ident) -> InstructionId {
        let symbol = self.block_data_mut(block).symbol_table.0.get(&ident);

        match symbol {
            Some(id) => id.to_owned(),
            None => {
                let id = self.lookup_symbol(block, &ident);
                self.block_data_mut(block).symbol_table.0.insert(ident.clone(), id);
                id
            }
        }
    }

    pub fn get_constant(&mut self, value: i32) -> InstructionId {
        for instruction in &self.get_block(BlockId(0)).statements {
            if let InstructionKind::Constant(c) = instruction.kind {
                if c == value {
                    return instruction.id;
                }
            }
        }

        let new_instruction = Instruction {
            kind: InstructionKind::Constant(value),
            dominating_instruction: None,
            id: self.issue_instruction_id(),
        };
        self.basic_blocks[0]
            .statements
            .push(new_instruction.clone());
        new_instruction.id
    }

    pub fn set_terminator(&mut self, block: BlockId, terminator: Terminator) {
        if self.block_data_mut(block).terminator.is_some() {
            panic!();
        }
        self.block_data_mut(block).terminator = Some(terminator);
    }

    pub fn goto(&mut self, block: BlockId, target: BlockId) {
        self.set_terminator(block, Terminator::Goto(target))
    }

    pub(crate) fn set_symbol(&mut self, block: BlockId, ident: Ident, id: InstructionId) {
        self.block_data_mut(block).symbol_table.set(ident, id);
    }

    pub(crate) fn delete(&mut self, start: BlockId) {
        warn!(
            "Deleting blocks {:?} -> {:?}",
            start,
            self.basic_blocks.len()
        );
        self.basic_blocks.drain(start.0..self.basic_blocks.len());
    }

    pub(crate) fn get_phi_instructions(&self, header_block: BlockId) -> Vec<InstructionId> {
        self.get_block(header_block)
            .header
            .iter()
            .filter_map(|header_instruction| match header_instruction.kind {
                HeaderStatementKind::Kill(_) => None,
                HeaderStatementKind::Phi(_, _) => Some(header_instruction.id),
                HeaderStatementKind::Param(_) => None,
            })
            .collect()
    }

    // TODO this is way too expensive
    fn get_instruction(&self, ins: InstructionId) -> Option<Instruction> {
        self.basic_blocks
            .iter()
            .flat_map(|it| it.statements.clone())
            .find(|it| it.id == ins)
    }
}
