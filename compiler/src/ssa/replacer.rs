// use std::collections::HashMap;

// use log::error;

// use crate::ssa::types::Terminator;

// use super::{
//     cfg::ControlFlowGraph,
//     types::{BlockId, HeaderStatementKind, InstructionId},
// };

// struct Zone {
//     entry: BlockId,
//     bounds: Vec<BlockId>,
// }

// pub(crate) struct Replacer<'a> {
//     cfg: &'a mut ControlFlowGraph,
//     zone: Zone,
//     fix_headers: bool,
//     visited: Vec<BlockId>,
//     replacements: HashMap<InstructionId, InstructionId>,
// }

// impl<'a> Replacer<'a> {
//     pub(crate) fn fix_while(
//         cfg: &'a mut ControlFlowGraph,
//         header: BlockId,
//         follow: BlockId,
//         replacements: HashMap<InstructionId, InstructionId>,
//     ) -> Self {
//         Replacer {
//             cfg,
//             zone: Zone {
//                 entry: header,
//                 bounds: vec![follow],
//             },
//             fix_headers: true,
//             visited: Vec::new(),
//             replacements,
//         }
//     }

//     fn do_replace(&mut self, block: BlockId) {
//         error!("Replacing at {:?}", block);
//         if self.zone.bounds.contains(&block) || self.visited.contains(&block) {
//             error!("Stopping replacement at {:?}", block);
//             return;
//         }

//         let current_block = self.cfg.block_data_mut(block);
//         // TODO move this function into this struct
//         current_block.replace(&self.replacements);

//         self.visited.push(block);

//         match current_block.terminator {
//             Some(Terminator::Goto(target)) => self.do_replace(target),
//             Some(Terminator::ConditionalBranch {
//                 condition: _,
//                 target,
//                 fallthrough,
//             }) => {
//                 self.do_replace(target);
//                 self.do_replace(fallthrough);
//             }
//             None => {
//                 error!("No terminator at block {:?}", block);
//             }
//         };
//     }
//     fn replaced(id: InstructionId, replacements: &HashMap<InstructionId, InstructionId>) -> InstructionId {
//         replacements.get(&id).copied().unwrap_or(id)
//     }

//     fn fix_header(&mut self) {
//         // the replacement operation is rather zealous and will also replaces the old value in the phi statement.
//         // TODO we should probably push this into the replacement algo somehow, and also make a `Replacer` struct or something.
//         let mut inverted_replacements = HashMap::new();
//         for (old, new) in &self.replacements {
//             inverted_replacements.insert(*new, *old);
//         }

//         let current_block = self.cfg.block_data_mut(self.zone.entry);
//         for header in current_block.header.iter_mut() {
//             match header.kind {
//                 HeaderStatementKind::Phi(l, r) if inverted_replacements.contains_key(&l) || inverted_replacements.contains_key(&r) => {
//                     header.kind = HeaderStatementKind::Phi(Self::replaced(l, &inverted_replacements), Self::replaced(r, &inverted_replacements));
//                 }
//                 HeaderStatementKind::Kill(_) | _ => {}
//             }
//         }
//         if let Some(Terminator::ConditionalBranch {
//             ref mut condition,
//             target: _,
//             fallthrough: _,
//         }) = &mut current_block.terminator
//         {
//             error!(
//                 "Replacing comparison value {:?} -> {:?}",
//                 condition.value,
//                 Self::replaced(condition.value, &self.replacements)
//             );
//             (*condition).value = Self::replaced(condition.value, &self.replacements);
//         };
//     }
//     pub(crate) fn replace(&mut self) {
//         self.do_replace(self.zone.entry);

//         if self.fix_headers {
//             self.fix_header();
//         }
//     }
// }