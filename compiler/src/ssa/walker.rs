use std::collections::HashSet;

use crate::ssa::types::Terminator;

use super::{
    cfg::ControlFlowGraph,
    types::{BlockId, InstructionId},
};

struct Zone {
    entry: BlockId,
    bounds: Vec<BlockId>,
}

pub(crate) struct KillFinder<'a> {
    cfg: &'a ControlFlowGraph,
    zone: Zone,
    visited: Vec<BlockId>,
    kills: HashSet<InstructionId>,
}

impl<'a> KillFinder<'a> {
    pub(crate) fn new(cfg: &'a mut ControlFlowGraph, header: BlockId, follow: BlockId) -> Self {
        KillFinder {
            cfg,
            zone: Zone {
                entry: header,
                bounds: vec![follow],
            },
            visited: Vec::new(),
            kills: HashSet::new(),
        }
    }

    fn edges(&mut self, block: BlockId) -> Vec<BlockId> {
        let block = self.cfg.get_block(block);
        match &block.terminator {
            Some(Terminator::Goto(target)) => vec![*target],
            Some(Terminator::ConditionalBranch {
                condition: _,
                target,
                fallthrough,
            }) => vec![*target, *fallthrough],
            None => Vec::new(),
        }
    }

    fn find_kills(&self, block: BlockId) -> HashSet<InstructionId> {
        self.cfg
            .get_block(block)
            .statements
            .iter()
            .filter_map(|it| match &it.kind {
                super::types::InstructionKind::Store(store) => Some(store.base),
                _ => None,
            })
            .collect()
    }

    fn visit(&mut self, block: BlockId) {
        if self.zone.bounds.contains(&block) || self.visited.contains(&block) {
            return;
        }
        self.visited.push(block);
        self.kills.extend(self.find_kills(block).iter());
        for edge in self.edges(block) {
            self.visit(edge);
        }
    }

    pub fn collect_kills(mut self) -> HashSet<InstructionId> {
        self.visit(self.zone.entry);
        return self.kills;
    }
}
