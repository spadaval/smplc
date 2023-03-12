use std::{
    collections::HashMap,
    fmt::Display,
    mem::{self, Discriminant},
};

use derive_new::new;
use log::{debug, info};

//TODO remove a bunch of the `pub` declarations and provide controlled access methods instead
use crate::{
    parser::{Block, Designator, Expression, ProgramForest, Relation, Statement},
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
    kind: ComparisonKind,
    value: InstructionId,
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
}

#[derive(Debug, Clone)]
pub struct SymbolTable(pub HashMap<Ident, InstructionId>);

#[derive(Debug, PartialEq, Clone)]
pub struct DominanceTable {
    expressions: HashMap<Discriminant<Expression>, InstructionId>,
}

impl DominanceTable {
    fn add_expression(&mut self, expr: &Expression, id: InstructionId) {
        self.expressions.insert(mem::discriminant(expr), id);
    }

    fn get_expression(&self, expr: &Expression) -> Option<InstructionId> {
        self.expressions.get(&mem::discriminant(expr)).copied()
    }

    fn new() -> Self {
        return DominanceTable {
            expressions: Default::default(),
        };
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlockData {
    pub header: Vec<HeaderInstruction>,
    pub statements: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub symbol_table: SymbolTable,
    pub dominance_table: DominanceTable,
}

impl BasicBlockData {
    fn new() -> Self {
        Self {
            header: Vec::new(),
            statements: Vec::new(),
            terminator: None,
            symbol_table: SymbolTable(Default::default()),
            dominance_table: DominanceTable::new(),
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct InstructionId(pub usize);

#[derive(Debug)]
pub struct ControlFlowGraph {
    basic_blocks: Vec<BasicBlockData>,
    instruction_count: usize,
}

impl ControlFlowGraph {
    fn new() -> Self {
        let mut cfg = ControlFlowGraph {
            basic_blocks: Vec::new(),
            instruction_count: 0,
        };
        let const_block_id = cfg.add_block(BasicBlockData::new());
        cfg.get_constant(0);

        let start_block_id = cfg.add_block(BasicBlockData::new());

        cfg.goto(const_block_id, start_block_id);

        cfg
    }

    fn start_block(&self) -> BlockId {
        assert!(self.basic_blocks.len() >= 2);
        BlockId(1)
    }

    fn get_block(&self, block_id: BlockId) -> &BasicBlockData {
        &self.basic_blocks[block_id.0]
    }

    fn add_block(&mut self, block: BasicBlockData) -> BlockId {
        let new_block_id = self.basic_blocks.len();
        self.basic_blocks.push(block);
        BlockId(new_block_id)
    }

    fn zero_value(&mut self) -> InstructionId {
        self.get_constant(0)
    }

    fn update_symbol(&mut self, block: BlockId, ident: Ident, val: InstructionId) {
        self.basic_blocks[block.0].symbol_table.0.insert(ident, val);
    }

    fn issue_instruction_id(&mut self) -> InstructionId {
        let new_id = InstructionId(self.instruction_count);
        self.instruction_count += 1;
        new_id
    }

    fn block_data_mut(&mut self, blk: BlockId) -> &mut BasicBlockData {
        &mut self.basic_blocks[blk.0]
    }

    fn add_instruction(&mut self, block: BlockId, instruction: InstructionKind) -> InstructionId {
        let new_id = self.issue_instruction_id();
        let data = self.block_data_mut(block);
        let instruction = Instruction {
            kind: instruction,
            id: new_id,
            dominating_instruction: None,
        };

        data.statements.push(instruction);
        new_id
    }

    fn add_header_statement(
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

    fn lookup_symbol(&mut self, block: BlockId, ident: &Ident) -> InstructionId {
        let references: Vec<InstructionId> = self
            .predeccessors(block)
            .iter()
            .filter_map(|block_id| {
                let data = self.get_block(*block_id);
                data.symbol_table.0.get(ident)
            })
            .copied()
            .collect();

        if references.len() == 1 {
            *references.first().unwrap()
        } else if references.is_empty() {
            // implicit initialization of undefined variables to zero
            self.zero_value()
        } else {
            assert!(references.len() == 2);
            let phi = HeaderStatementKind::Phi(references[0], references[1]);
            self.add_header_statement(block, phi)
        }
    }

    // TODO make this return an error if a symbol can't be resolved correctly (for while loops)
    fn resolve_symbol(&mut self, block: BlockId, ident: Ident) -> InstructionId {
        let symbol = self.block_data_mut(block).symbol_table.0.get(&ident);

        match symbol {
            Some(id) => id.to_owned(),
            None => {
                let id = self.lookup_symbol(block, &ident);
                self.block_data_mut(block).symbol_table.0.insert(ident, id);
                id
            }
        }
    }

    fn get_constant(&mut self, value: i32) -> InstructionId {
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
        info!("Issue constant with ID {:?}", new_instruction.id);
        self.basic_blocks[0]
            .statements
            .push(new_instruction.clone());
        new_instruction.id
    }

    fn new_block(&mut self) -> BlockId {
        let data = BasicBlockData::new();
        self.basic_blocks.push(data);
        BlockId(self.basic_blocks.len() - 1)
    }

    fn set_terminator(&mut self, block: BlockId, terminator: Terminator) {
        if self.block_data_mut(block).terminator.is_some() {
            panic!();
        }
        self.block_data_mut(block).terminator = Some(terminator);
    }

    fn goto(&mut self, block: BlockId, target: BlockId) {
        self.set_terminator(block, Terminator::Goto(target))
    }

    pub fn blocks(&self) -> impl Iterator<Item = (BlockId, BasicBlockData)> {
        self.basic_blocks
            .clone()
            .into_iter()
            .enumerate()
            .map(|(id, data)| (BlockId(id), data))
    }

    fn lookup_expression(&self, block: BlockId, expr: &Expression) -> Option<InstructionId> {
        self.get_block(block).dominance_table.get_expression(expr)
    }

    fn save_expression(&mut self, block: BlockId, expr: &Expression, id: InstructionId) {
        self.block_data_mut(block)
            .dominance_table
            .add_expression(expr, id);
    }
}

impl Display for BasicBlockData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.statements.is_empty() {
            write!(f, "empty block")
        } else {
            for ele in &self.statements {
                write!(f, " {} ", ele.id.0).unwrap();
            }
            Ok(())
        }
    }
}

// Expression cannot lower across multiple blocks
// TODO CSE
fn lower_expression(
    cfg: &mut ControlFlowGraph,
    block: BlockId,
    expr: &Expression,
) -> InstructionId {
    let existing_expression: Option<InstructionId> = cfg.lookup_expression(block, expr);
    match existing_expression {
        Some(id) => id,
        None => {
            let id = do_lower_expression(cfg, block, &expr);
            cfg.save_expression(block, expr, id);
            id
        }
    }
}

fn do_lower_expression(
    cfg: &mut ControlFlowGraph,
    block: BlockId,
    expr: &Expression,
) -> InstructionId {
    match expr {
        Expression::Constant(c) => cfg.get_constant(*c),
        // a lookup of an uninitialized variable returns the ZERO constant
        Expression::Identifier(ident) => cfg.resolve_symbol(block, ident.clone()),
        Expression::Add(l, r) => {
            let add = InstructionKind::BasicOp(
                BasicOpKind::Add,
                lower_expression(cfg, block, l),
                lower_expression(cfg, block, r),
            );
            cfg.add_instruction(block, add)
        }
        Expression::Subtract(l, r) => {
            let sub = InstructionKind::BasicOp(
                BasicOpKind::Subtract,
                lower_expression(cfg, block, l),
                lower_expression(cfg, block, r),
            );
            cfg.add_instruction(block, sub)
        }
        Expression::Multiply(l, r) => {
            let mul = InstructionKind::BasicOp(
                BasicOpKind::Multiply,
                lower_expression(cfg, block, l),
                lower_expression(cfg, block, r),
            );
            cfg.add_instruction(block, mul)
        }
        Expression::Divide(l, r) => {
            let div = InstructionKind::BasicOp(
                BasicOpKind::Divide,
                lower_expression(cfg, block, l),
                lower_expression(cfg, block, r),
            );
            cfg.add_instruction(block, div)
        }
        Expression::Call(call) => {
            info!("Lowering call: {}", call.function_name.0);
            if call.function_name.0 == "InputNum" {
                cfg.add_instruction(block, InstructionKind::Read)
            } else if call.function_name.0 == "OutputNum" {
                let arg = call.arguments.first().unwrap();
                let val = lower_expression(cfg, block, arg);
                cfg.add_instruction(block, InstructionKind::Write(val))
            } else {
                todo!()
            }
        }
    }
}

fn lower_relation(cfg: &mut ControlFlowGraph, block: BlockId, relation: Relation) -> Comparison {
    // TODO make this a little more intelligent
    match relation.compare_op {
        crate::tokenizer::Token::LessThan => {
            let expr = Expression::Subtract(Box::new(relation.left), Box::new(relation.right));
            Comparison {
                kind: ComparisonKind::LtZero,
                value: lower_expression(cfg, block, &expr),
            }
        }
        crate::tokenizer::Token::GreaterThan => {
            let expr = Expression::Subtract(Box::new(relation.left), Box::new(relation.right));
            Comparison {
                kind: ComparisonKind::GtZero,
                value: lower_expression(cfg, block, &expr),
            }
        }
        crate::tokenizer::Token::LessThanEqual => todo!(),
        crate::tokenizer::Token::GreaterThanEqual => todo!(),
        _ => panic!(),
    }
}

fn lower_statement(cfg: &mut ControlFlowGraph, block: BlockId, statement: Statement) -> BlockId {
    match statement {
        Statement::Assign(Designator::Ident(ident), expr) => {
            let val = lower_expression(cfg, block, &expr);
            cfg.update_symbol(block, ident, val);
            block
        }
        Statement::Assign(Designator::ArrayIndex(_, _), _expr) => todo!(),
        Statement::If {
            ref condition,
            ref body,
            ref else_body,
        } => {
            debug!("Lowering statement {:?}", statement.clone());
            let header_block = block;

            let main_body = cfg.new_block();
            let main_body_end = lower_block(cfg, main_body, body.clone());

            let follow_block = cfg.new_block();

            let condition = lower_relation(cfg, header_block, condition.clone());

            match else_body {
                Some(else_body) => {
                    let else_body_start = cfg.new_block();

                    cfg.set_terminator(
                        header_block,
                        Terminator::ConditionalBranch {
                            condition,
                            target: main_body,
                            fallthrough: else_body_start,
                        },
                    );
                    let else_body_end = lower_block(cfg, else_body_start, else_body.clone());
                    debug!(
                        "Lowered else_body from block {} to block {}",
                        else_body_start.0, else_body_end.0
                    );
                    cfg.goto(main_body_end, follow_block);
                    cfg.goto(else_body_end, follow_block);
                }
                None => {
                    debug!("Lowered condition to block {}", header_block.0);
                    debug!(
                        "Add edge {} -(target)-> {}, {} -(fallthrough)-> {}",
                        header_block.0, main_body.0, header_block.0, follow_block.0
                    );
                    cfg.set_terminator(
                        header_block,
                        Terminator::ConditionalBranch {
                            condition,
                            target: main_body,
                            fallthrough: follow_block,
                        },
                    );

                    cfg.goto(main_body, follow_block);
                }
            }
            follow_block
        }
        Statement::While { condition, body } => {
            debug!("Lowering while statement ");

            let header_block = block;
            let condition = lower_relation(cfg, header_block, condition.clone());

            let main_body = cfg.new_block();
            let main_body_end = lower_block(cfg, main_body, body.clone());

            let follow_block = cfg.new_block();

            cfg.set_terminator(
                header_block,
                Terminator::ConditionalBranch {
                    condition,
                    target: main_body,
                    fallthrough: follow_block,
                },
            );
            debug!(
                "Add edge {} -(target)-> {}, {} -(fallthrough)-> {}",
                header_block.0, main_body.0, header_block.0, follow_block.0
            );

            cfg.goto(main_body_end, header_block);

            follow_block
        }
        Statement::Call(call) => {
            if call.function_name.0 == "OutputNum" {
                let arg = call
                    .arguments
                    .first()
                    .expect("A call to OutputNum should have at least one argument");
                let ins = lower_expression(cfg, block, &arg);
                cfg.add_instruction(block, InstructionKind::Write(ins));
                block
            } else {
                todo!()
            }
        }
    }
}

fn lower_block(cfg: &mut ControlFlowGraph, mut blk: BlockId, block: Block) -> BlockId {
    for statement in block.0 {
        blk = lower_statement(cfg, blk, statement);
    }
    blk
}

pub fn lower_program(forest: ProgramForest) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    let mut block = cfg.start_block();
    for statement in forest.roots {
        block = lower_statement(&mut cfg, block, statement);
    }
    cfg
}

#[cfg(test)]
mod tests {
    use pretty_env_logger::env_logger;

    use crate::parser::parse;

    use super::lower_program;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_assign() {
        let forest = parse(crate::SourceFile::new("let a <- a + 1"));
        let cfg = lower_program(forest);
        println!("{cfg:#?}");
    }

    #[test]
    fn test_if() {
        init();
        let program = r"
            let a <- 0;
            let b <- 10;
            if a < b
            then 
                let b <- b + a;
            fi
        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let _cfg = lower_program(forest);
        //println!("{cfg:#?}");
        //cfg.render();
    }
}
