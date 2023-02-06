use std::{collections::HashMap, fmt::Display};

use termgraph::ValueFormatter;

use crate::{
    parser::{Block, Designator, Expression, ProgramForest, Relation, Statement},
    tokenizer::Ident,
};

// This design is shamelessly copied from `rustc`
#[derive(Debug, Clone, Copy)]
struct BasicBlock(usize);

#[derive(Debug)]
struct Instruction {
    kind: InstructionKind,
    dominating_instruction: Option<InstructionId>,
    id: InstructionId,
}
#[derive(Debug)]
enum BasicOpKind {
    Add,
    Subtract,
}
#[derive(Debug)]
enum ImmediateOpKind {
    AddI,
    SubtractI,
}

#[derive(Debug)]
enum InstructionKind {
    Constant(f32),
    BasicOp(BasicOpKind, InstructionId, InstructionId),
    ImmediateOp(ImmediateOpKind, InstructionId, InstructionId),
}

#[derive(Debug)]
struct Comparison {
    kind: ComparisonKind,
    value: InstructionId,
}
// For some reason DLX has all of these instructions. Might as well use them.
// They are easy enough to emulate.
#[derive(Debug)]
enum ComparisonKind {
    LtZero,
    LteZero,
    GtZero,
    GteZero,
    EqZero,
    NeZero,
}

#[derive(Debug)]
enum Terminator {
    Goto(BasicBlock),
    ConditionalBranch {
        condition: Comparison,
        target: BasicBlock,
        fallthrough: BasicBlock,
    },
}

#[derive(Debug)]
enum HeaderStatement {
    // TODO this is probably wrong
    Kill(InstructionId),
    Phi(InstructionId, InstructionId),
}

#[derive(Debug)]
struct SymbolTable(HashMap<Ident, InstructionId>);

#[derive(Debug)]
struct BasicBlockData {
    header: Vec<HeaderStatement>,
    statements: Vec<Instruction>,
    terminator: Option<Terminator>,
    symbol_table: SymbolTable,
}

impl BasicBlockData {
    fn new() -> Self {
        Self {
            header: Vec::new(),
            statements: Vec::new(),
            terminator: None,
            symbol_table: SymbolTable(Default::default()),
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
struct InstructionId(usize);

#[derive(Debug)]
pub struct ControlFlowGraph {
    basic_blocks: Vec<BasicBlockData>,
    instruction_count: usize,
}

impl ControlFlowGraph {
    fn get_block(&self, block_id: BasicBlock) -> &BasicBlockData {
        return &self.basic_blocks[block_id.0];
    }
    fn add_block(&mut self, block: BasicBlockData) -> BasicBlock {
        let new_block_id = self.basic_blocks.len();
        self.basic_blocks.push(block);
        BasicBlock(new_block_id)
    }
    fn update_block(&mut self, block: BasicBlock, block_data: BasicBlockData) {
        self.basic_blocks[block.0] = block_data;
    }

    fn new() -> Self {
        let mut cfg = ControlFlowGraph {
            basic_blocks: Vec::new(),
            instruction_count: 0,
        };
        let mut const_block = BasicBlockData::new();
        let instruction = Instruction {
            kind: InstructionKind::Constant(0.0),
            dominating_instruction: None,
            id: InstructionId(0),
        };

        const_block.statements.push(instruction);
        cfg.add_block(const_block);
        let mut start_block = BasicBlockData::new();
        cfg.add_block(start_block);

        cfg
    }

    fn zero_value(&self) -> InstructionId {
        return InstructionId(0);
    }

    fn start_block(&self) -> BasicBlock {
        assert!(self.basic_blocks.len() >= 2);
        BasicBlock(1)
    }

    fn update_symbol(&mut self, block: BasicBlock, ident: Ident, val: InstructionId) {
        self.basic_blocks[block.0].symbol_table.0.insert(ident, val);
    }

    fn get_symbol(&mut self, block: BasicBlock, ident: Ident) -> Option<InstructionId> {
        self.basic_blocks[block.0]
            .symbol_table
            .0
            .get(&ident)
            .copied()
    }

    fn add_constant(&mut self, c: f32) -> InstructionId {
        // TODO dedup constants
        let new_instruction = Instruction {
            kind: InstructionKind::Constant(c),
            dominating_instruction: None,
            id: InstructionId(0),
        };
        self.basic_blocks[0].statements.push(new_instruction);
        return InstructionId(0);
    }

    fn issue_instruction_id(&mut self) -> InstructionId {
        let new_id = InstructionId(self.instruction_count);
        self.instruction_count += 1;
        return new_id;
    }

    fn block_data_mut(&mut self, blk: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[blk.0]
    }

    fn add_instruction(
        &mut self,
        block: BasicBlock,
        instruction: InstructionKind,
    ) -> InstructionId {
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

    fn new_block(&mut self) -> BasicBlock {
        let data = BasicBlockData::new();
        self.basic_blocks.push(data);
        BasicBlock(self.basic_blocks.len() - 1)
    }

    fn set_terminator(&mut self, block: BasicBlock, terminator: Terminator) {
        self.block_data_mut(block).terminator = Some(terminator);
    }

    fn goto(&mut self, block: BasicBlock, target: BasicBlock) {
        self.set_terminator(block, Terminator::Goto(target))
    }

    fn render(&self) {
        use termgraph::{Config, DirectedGraph, IDFormatter};
        let mut graph = DirectedGraph::new();

        graph.add_nodes(self.basic_blocks.iter().enumerate());
        //graph.add_nodes([(0, "first"), (1, "second"), (2, "third")]);

        let edges = self.basic_blocks.iter().enumerate().flat_map(
            |(block_id, block_data)| -> Vec<(usize, usize)> {
                match &block_data.terminator {
                    Some(Terminator::Goto(target)) => vec![(block_id, target.0)],
                    Some(Terminator::ConditionalBranch {
                        condition,
                        target,
                        fallthrough,
                    }) => vec![(block_id, target.0), (block_id, fallthrough.0)],
                    None => Vec::new(),
                }
            },
        );
        graph.add_edges(edges);

        let value_config = Config::new(ValueFormatter::new(), 3).default_colors();
        termgraph::display(&graph, &value_config);
    }
}

impl Display for BasicBlockData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.statements.len() == 0 {
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
fn lower_expression(
    cfg: &mut ControlFlowGraph,
    block: BasicBlock,
    expr: Expression,
) -> InstructionId {
    match expr {
        Expression::Constant(c) => cfg.add_constant(c),
        // a lookup of an uninitialized variable returns the ZERO constant
        Expression::Identifier(ident) => cfg.get_symbol(block, ident).unwrap_or(cfg.zero_value()),
        Expression::Add(l, r) => {
            let add = InstructionKind::BasicOp(
                BasicOpKind::Add,
                lower_expression(cfg, block, *l),
                lower_expression(cfg, block, *r),
            );
            cfg.add_instruction(block, add)
        }
        Expression::Subtract(l, r) => {
            let sub = InstructionKind::BasicOp(
                BasicOpKind::Subtract,
                lower_expression(cfg, block, *l),
                lower_expression(cfg, block, *r),
            );
            cfg.add_instruction(block, sub)
        }
        Expression::Multiply(_, _) => todo!(),
        Expression::Divide(_, _) => todo!(),
    }
}

fn lower_relation(cfg: &mut ControlFlowGraph, block: BasicBlock, relation: Relation) -> Comparison {
    // TODO make this a little more intelligent
    match relation.compare_op {
        crate::tokenizer::Token::LessThan => {
            let expr = Expression::Subtract(Box::new(relation.left), Box::new(relation.right));
            Comparison {
                kind: ComparisonKind::LtZero,
                value: lower_expression(cfg, block, expr),
            }
        }
        crate::tokenizer::Token::GreaterThan => {
            let expr = Expression::Subtract(Box::new(relation.left), Box::new(relation.right));
            Comparison {
                kind: ComparisonKind::GtZero,
                value: lower_expression(cfg, block, expr),
            }
        }
        crate::tokenizer::Token::LessThanEqual => todo!(),
        crate::tokenizer::Token::GreaterThanEqual => todo!(),
        _ => panic!(),
    }
}

fn lower_statement(
    cfg: &mut ControlFlowGraph,
    block: BasicBlock,
    statement: Statement,
) -> BasicBlock {
    match statement {
        Statement::Assign(Designator::Ident(ident), expr) => {
            let val = lower_expression(cfg, block, expr);
            cfg.update_symbol(block, ident, val);
            block
        }
        Statement::Assign(Designator::ArrayIndex(_, _), expr) => todo!(),
        Statement::If {
            condition,
            body,
            else_body,
        } => {
            let header_block = block;

            let follow_block = cfg.new_block();

            let new_block = cfg.new_block();
            let main_body_blk = lower_block(cfg, new_block, body);
            match else_body {
                Some(else_body) => {
                    let else_body_blk = cfg.new_block();

                    let condition = lower_relation(cfg, header_block, condition);
                    cfg.set_terminator(
                        header_block,
                        Terminator::ConditionalBranch {
                            condition,
                            target: main_body_blk,
                            fallthrough: else_body_blk,
                        },
                    );
                    let else_body_blk = lower_block(cfg, else_body_blk, else_body);
                    cfg.goto(else_body_blk, header_block);
                }
                None => {
                    let condition = lower_relation(cfg, header_block, condition);
                    cfg.set_terminator(
                        header_block,
                        Terminator::ConditionalBranch {
                            condition,
                            target: main_body_blk,
                            fallthrough: follow_block,
                        },
                    );

                    cfg.goto(main_body_blk, header_block);
                }
            }
            follow_block
        }
        Statement::While { condition, body } => todo!(),
    }
}

fn lower_block(cfg: &mut ControlFlowGraph, mut blk: BasicBlock, block: Block) -> BasicBlock {
    for statement in block.0 {
        blk = lower_statement(cfg, blk, statement);
    }
    blk
}

#[allow(dead_code)]
pub fn lower_program(forest: ProgramForest) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    let block = cfg.start_block();
    for statement in forest.roots {
        lower_statement(&mut cfg, block, statement);
    }
    cfg
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse, ProgramForest};

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
            else 
                let a <- a + b;
            fi
        ";
        let forest = parse(crate::SourceFile::new(program));
        //println!("{forest:#?}");
        let cfg = lower_program(forest);
        //println!("{cfg:#?}");
        //cfg.render();
    }
}
