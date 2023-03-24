use log::{error, info, warn};

use crate::{
    parser::{Variable, VariableType},
    ssa::flags::STORE_RESIDUAL,
    tokenizer::Ident,
};

use super::{
    flags::{CONST_COMPTIME, ELIMINATE_DOMINANCE},
    types::*,
};

#[derive(Debug)]
pub struct ControlFlowGraph {
    basic_blocks: Vec<BasicBlockData>,
    instruction_count: usize,
    variables: Vec<Variable>,
}

impl ControlFlowGraph {
    pub fn new(globals: &Vec<Variable>, linkage: Linkage) -> Self {
        let mut cfg = ControlFlowGraph {
            basic_blocks: Vec::new(),
            instruction_count: 0,
            variables: globals.to_owned(),
        };
        let const_block_id = cfg.new_start_block();
        cfg.get_constant(0);

        for var in globals {
            let header = HeaderStatementKind::Variable(linkage, var.clone());
            let id = cfg.add_header_statement(const_block_id, header);
            cfg.set_symbol(const_block_id, var, id);
        }

        let start_block_id = cfg.new_block(const_block_id);

        cfg.goto(const_block_id, start_block_id);

        cfg
    }

    pub fn start_block(&self) -> BlockId {
        assert!(self.basic_blocks.len() >= 2);
        BlockId(1)
    }

    pub fn const_block_id(&self) -> BlockId {
        BlockId(0)
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
        // println!("created block {}: {:?}", self.basic_blocks.len(), data.dominance_table);
        // println!("using {:?}", dominating_block_data.dominance_table);

        self.basic_blocks.push(data);
        BlockId(self.basic_blocks.len() - 1)
    }

    pub fn propagate_dominance(&mut self, start_block: BlockId) {
        let end_blocks = match self.get_block(start_block).terminator.clone() {
            Some(Terminator::Goto(target)) => vec![target],
            Some(Terminator::ConditionalBranch {
                condition: _,
                target,
                fallthrough,
            }) => vec![target, fallthrough],
            None => panic!("Tried to propagate dominance with no terminator"),
        };
        for end_block in end_blocks {
            self.propagate_dominance_to(start_block, end_block);
        }
    }
    pub fn propagate_dominance_to(&mut self, start_block: BlockId, end_block: BlockId) {
        let table = &self.get_block(end_block).dominance_table.clone();
        let start = self.block_data_mut(start_block);
        start.dominance_table.copy_from(table);
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
        if let Some(ins) = self.try_eliminate(block, &instruction) {
            return ins;
        }

        let new_id = self.issue_instruction_id();

        let dominating_instruction = self
            .get_block(block)
            .dominance_table
            .get_dominating_instruction(&instruction);

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

    fn try_eliminate_ssa(
        &mut self,
        block: BlockId,
        instruction: &InstructionKind,
    ) -> Option<InstructionId> {
        //const elimination
        if CONST_COMPTIME {
            match instruction {
                InstructionKind::BasicOp(op, l, r) => {
                    let l = self.get_instruction(*l).map(|it| it.kind);
                    let r = self.get_instruction(*r).map(|it| it.kind);
                    if let Some(InstructionKind::Constant(left)) = l && let Some(InstructionKind::Constant(right)) = r {
                    let val = match op {
                        BasicOpKind::Add => left+right,
                        BasicOpKind::Addi => left+right,
                        BasicOpKind::Subtract => left-right,
                        BasicOpKind::Multiply => left*right,
                        BasicOpKind::Divide => left/right,
                    };
                    return Some(self.get_constant(val));
                };
                }
                _ => {}
            };
        }

        if ELIMINATE_DOMINANCE {
            // dominance elimination
            let dominating_instruction = self
                .get_block(block)
                .dominance_table
                .get_dominating_instruction(instruction)
                .and_then(|it| self.get_instruction(it));

            let mut curr_dom_instruction = dominating_instruction;

            while let Some(ref ins) = curr_dom_instruction {
                if ins.kind == *instruction {
                    return Some(ins.id);
                } else {
                    curr_dom_instruction = ins
                        .dominating_instruction
                        .and_then(|it| self.get_instruction(it));
                    //assert!(curr_dom_instruction != Some(ins.clone()));
                }
            }
        }
        None
    }

    fn try_eliminate_load(
        &mut self,
        block: BlockId,
        load_to_eliminate: &Load,
    ) -> Option<InstructionId> {
        let mut next_instruction = self.get_block(block).dominance_table.get_load_store();

        while let Some(ref next) = next_instruction {
            let next_header = self.get_header(*next);
            let next_ins = self.get_instruction(*next);
            info!("Next header: {:?}", next_header);
            info!("Next ins: {:?}", next_ins);

            if let Some(header) = next_header {
                match header.kind {
                    HeaderStatementKind::Kill(kill) => {
                        if load_to_eliminate.base == kill {
                            info!("Not eliminating [killed] {load_to_eliminate:?}");
                            return None;
                        } else {
                            next_instruction = header.dominator;
                        }
                    }
                    HeaderStatementKind::Phi(_, _) => {
                        error!(
                            "Broken killchain at {:?} for {:?}",
                            header, load_to_eliminate
                        );
                        return None;
                    }
                    HeaderStatementKind::Param(_) => {
                        error!(
                            "Broken killchain at {:?} for {:?}",
                            header, load_to_eliminate
                        );
                        return None;
                    }
                    _ => unreachable!(),
                }
            } else if let Some(next_ins) = next_ins {
                match next_ins.kind {
                    InstructionKind::Load(load) => {
                        if *load_to_eliminate == load {
                            info!("Eliminating {load_to_eliminate:?}");
                            return Some(next_ins.id);
                        } else {
                            info!("Move to {:?}", next_ins.dominating_instruction);
                            next_instruction = next_ins.dominating_instruction;
                        }
                    }
                    InstructionKind::Store(store) => {
                        if STORE_RESIDUAL && store.pointer == load_to_eliminate.pointer {
                            return Some(next_ins.id);
                        }
                        if load_to_eliminate.base == store.base {
                            return None;
                        } else {
                            info!("Move to {:?}", next_ins.dominating_instruction);
                            next_instruction = next_ins.dominating_instruction;
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
        None
    }

    fn try_eliminate(
        &mut self,
        block: BlockId,
        instruction: &InstructionKind,
    ) -> Option<InstructionId> {
        match instruction {
            InstructionKind::Constant(_) | InstructionKind::BasicOp(_, _, _) => {
                self.try_eliminate_ssa(block, instruction)
            }

            InstructionKind::Load(load) => self.try_eliminate_load(block, load),

            // anything that can have side-effects can't be eliminated
            // TODO eliminate pure functions?
            InstructionKind::Read
            | InstructionKind::Write(_)
            | InstructionKind::Return(_)
            | InstructionKind::Call(_, _)
            | InstructionKind::Store(_) => None,
        }
    }

    pub fn add_header_statement(
        &mut self,
        block: BlockId,
        instruction: HeaderStatementKind,
    ) -> InstructionId {
        let new_id = self.issue_instruction_id();

        let dominator = if let HeaderStatementKind::Kill(_) = &instruction {
            self.get_block(block).dominance_table.get_load_store()
        } else {
            None
        };

        let i = HeaderInstruction {
            kind: instruction,
            id: new_id,
            dominator,
        };

        if let HeaderStatementKind::Kill(_) = i.kind {
            self.block_data_mut(block)
                .dominance_table
                .register_kill(new_id);
        }
        let data = self.block_data_mut(block);
        data.header.push(i);
        new_id
    }

    //TODO cache this maybe?
    pub fn predeccessors(&self, block: BlockId) -> Vec<BlockId> {
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
    fn lookup_or_init_symbol(&mut self, block: BlockId, ident: &Ident) -> InstructionId {
        let current_block = self.get_block(block);
        if let Some(x) = current_block.symbol_table.get(ident) {
            return x;
        }

        let references: Vec<(BlockId, Option<InstructionId>)> = self
            .predeccessors(block)
            .iter()
            .map(|block_id| {
                let data = self.get_block(*block_id);
                (*block_id, data.symbol_table.get(ident))
            })
            .collect();

        if references.iter().all(|it| it.1.is_none()) {
            return self.init_zero(ident);
        }

        assert!(references.len() == 2);

        let phi = HeaderStatementKind::Phi(
            references[0].1.unwrap_or(self.init_zero(ident)),
            references[1].1.unwrap_or(self.init_zero(ident)),
        );
        self.add_header_statement(block, phi)
    }

    fn init_zero(&mut self, ident: &Ident) -> InstructionId {
        warn!("Init {:?} to zero", ident);
        let var = Variable {
            ident: ident.clone(),
            dtype: crate::parser::VariableType::I32,
        };
        let zero = self.zero_value();
        self.const_block().symbol_table.set(var, zero);
        return zero;
    }

    pub fn resolve_symbol(&mut self, block: BlockId, ident: &Ident) -> InstructionId {
        let symbol = self.block_data_mut(block).symbol_table.get(ident);

        match symbol {
            Some(id) => id.to_owned(),
            None => {
                let id = self.lookup_or_init_symbol(block, &ident);
                self.block_data_mut(block)
                    .symbol_table
                    .update_or_init(ident, id);
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
            panic!("Tried to overwrite a terminator");
        }
        self.block_data_mut(block).terminator = Some(terminator);
    }

    pub fn goto(&mut self, block: BlockId, target: BlockId) {
        self.set_terminator(block, Terminator::Goto(target));
    }

    pub(crate) fn set_symbol(&mut self, block: BlockId, var: &Variable, id: InstructionId) {
        self.block_data_mut(block)
            .symbol_table
            .set(var.to_owned(), id);
    }

    pub fn update_variable(
        &mut self,
        block: BlockId,
        var_to_update: &Variable,
        val: InstructionId,
    ) {
        let map = &mut self.basic_blocks[block.0].symbol_table.0;
        let existing = map
            .iter()
            .find(|(var, ins)| var.ident == var_to_update.ident);
        if let Some(a) = existing {
            assert!(a.0.dtype == var_to_update.dtype);
        }
        self.basic_blocks[block.0]
            .symbol_table
            .0
            .insert(var_to_update.clone(), val);
    }

    pub fn update_symbol(&mut self, block: BlockId, ident: &Ident, val: InstructionId) {
        self.basic_blocks[block.0].symbol_table.update(ident, val);
    }

    pub(crate) fn delete(&mut self, start: BlockId) {
        warn!(
            "Deleting blocks {:?} -> {:?}",
            start,
            self.basic_blocks.len()
        );
        self.basic_blocks.drain(start.0..self.basic_blocks.len());
    }

    fn is_skippable(&self, block: BlockId) -> bool {
        let block = self.get_block(block);
        block.is_empty() && matches!(block.terminator, Some(Terminator::Goto(_)))
    }

    pub fn skip_empty_blocks(&mut self) {
        for (block, data) in self.blocks() {
            if self.is_skippable(block) {
                self.block_data_mut(block).terminator = None;
                self.block_data_mut(block).dominating_block = None;
                let preds = self.predeccessors(block);
                let follow = data.terminator.unwrap();
                let follow = follow.into_goto();

                for pred in preds {
                    let pred_data = self.block_data_mut(pred);
                    match &mut pred_data.terminator {
                        Some(Terminator::Goto(target)) => {
                            if *target == block {
                                *target = follow;
                            }
                        }
                        Some(Terminator::ConditionalBranch {
                            condition: _,
                            target,
                            fallthrough,
                        }) => {
                            if *target == block {
                                *target = follow;
                            }
                            if *fallthrough == block {
                                *fallthrough = follow.clone();
                            }
                        }
                        None => panic!("wtf"),
                    }
                    let follow_data = self.block_data_mut(follow);
                    if follow_data.dominating_block == Some(block) {
                        follow_data.dominating_block = Some(pred);
                    }
                }
            }
        }
    }

    // fn get_phi_instructions(&self, header_block: BlockId) -> Vec<InstructionId> {
    //     self.get_block(header_block)
    //         .header
    //         .iter()
    //         .filter_map(|header_instruction| match header_instruction.kind {
    //             HeaderStatementKind::Kill(_) => None,
    //             HeaderStatementKind::Phi(_, _) => Some(header_instruction.id),
    //             HeaderStatementKind::Param(_) => None,
    //         })
    //         .collect()
    // }

    // TODO this is way too expensive
    pub fn get_instruction(&self, ins: InstructionId) -> Option<Instruction> {
        self.basic_blocks
            .iter()
            .flat_map(|it| it.statements.iter())
            .find(|it| it.id == ins)
            .cloned()
    }

    pub fn get_header(&self, ins: InstructionId) -> Option<HeaderInstruction> {
        self.basic_blocks
            .iter()
            .flat_map(|it| it.header.iter())
            .find(|it| it.id == ins)
            .cloned()
    }

    pub(crate) fn propagate_symbols(&mut self, block: BlockId, target: BlockId) {
        let symbols = self.get_block(block).symbol_table.clone();
        self.block_data_mut(target).symbol_table.copy_from(&symbols);
    }

    fn const_block(&mut self) -> &mut BasicBlockData {
        &mut self.basic_blocks[0]
    }

    pub(crate) fn resolve_type(&self, block: BlockId, ident: &Ident) -> Option<VariableType> {
        self.variables
            .iter()
            .find(|it| it.ident == *ident)
            .map(|it| it.dtype.clone())
    }

    pub(crate) fn register_param(&mut self, var: &Variable) {
        self.variables.push(var.clone());
    }
}
