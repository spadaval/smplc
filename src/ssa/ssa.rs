use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops::ControlFlow,
};

use log::{debug, error, info, warn};

use crate::{
    parser::{Block, Designator, Expression, Relation, Statement},
    tokenizer::Ident,
};

use super::{cfg::ControlFlowGraph, types::*};

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
            let expr = lower_expression(cfg, block, &expr);

            Comparison {
                kind: ComparisonKind::LtZero,
                value: expr,
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
            let condition = lower_relation(cfg, header_block, condition.clone());

            let main_body = cfg.new_block(header_block);
            let main_body_end = lower_block(cfg, main_body, body);

            let follow_block = cfg.new_block(header_block);

            match else_body {
                Some(else_body) => {
                    let else_body_start = cfg.new_block(header_block);

                    cfg.set_terminator(
                        header_block,
                        Terminator::ConditionalBranch {
                            condition,
                            target: main_body,
                            fallthrough: else_body_start,
                        },
                    );
                    let else_body_end = lower_block(cfg, else_body_start, else_body);
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
        Statement::While { condition, body } => lower_while(cfg, block, condition, body),
        Statement::Call(call) => {
            if call.function_name.0 == "OutputNum" {
                let arg = call
                    .arguments
                    .first()
                    .expect("A call to OutputNum should have at least one argument");
                let ins = lower_expression(cfg, block, arg);
                cfg.add_instruction(block, InstructionKind::Write(ins));
                block
            } else {
                todo!()
            }
        }
    }
}

fn try_lower_while_body(
    cfg: &mut ControlFlowGraph,
    header_block: BlockId,
    body: &Block,
) -> (BlockId, BlockId, Vec<Mutation>, Vec<Mutation>) {
    let old_symbols = &cfg.get_symbols(header_block).clone();

    let main_body = cfg.new_block(header_block);
    debug!("Lowering main body");
    let main_body_end = lower_block(cfg, main_body, body);
    info!(
        "Lowered main while body from {:?}->{:?}",
        main_body, main_body_end
    );

    let phis: Vec<InstructionId> = cfg
        .get_block(header_block)
        .header
        .iter()
        .map(|it| it.id)
        .collect();

    let new_symbols = cfg.get_symbols(main_body_end);

    let changed_symbols = SymbolTable::changed_symbols(old_symbols, new_symbols);

    let mut valid_mutations = Vec::new();
    let mut invalid_mutations = Vec::new();

    warn!("Existing phis: {:?}", phis); 
    for mutation in changed_symbols {
        if phis.contains(&mutation.old) {
            warn!("{:?} is valid", mutation);
            valid_mutations.push(mutation);
        } else {
            warn!("{:?} is invalid", mutation);
            invalid_mutations.push(mutation);
        }
    }

    (main_body, main_body_end, valid_mutations, invalid_mutations)
}

fn lower_while(
    cfg: &mut ControlFlowGraph,
    root_block: BlockId,
    condition: Relation,
    body: Block,
) -> BlockId {
    debug!("Lowering while statement ");
    let header_block = cfg.new_block(root_block);
    cfg.goto(root_block, header_block);
    // TODO extract this into a func that returns a Result
    let mut created_phis = HashMap::new();
    let mut i = 0;
    loop {
        if i == 2 {
            panic!();
        }
        i += 1;

        info!("Try lowering while body (iteration {})", i);
        let (main_body, main_body_end, valid_mutations, invalid_mutations) =
            try_lower_while_body(cfg, header_block, &body);

        warn!("Detected invalid mutations: {:?}", invalid_mutations);
        warn!("Detected valid mutations: {:?}", valid_mutations);


        if invalid_mutations.is_empty() {

            for mutation in valid_mutations {
                match created_phis.get(&mutation.ident) {
                    Some(instruction_id) => {
                        cfg.block_data_mut(header_block).update_phi(*instruction_id, mutation.new);

                    },
                    None => {},
                }
            }


            info!("While lowering complete");
            cfg.goto(main_body_end, header_block);
            let condition = lower_relation(cfg, header_block, condition);
            let follow_block = cfg.new_block(header_block);
            cfg.set_terminator(
                header_block,
                Terminator::ConditionalBranch {
                    condition: condition.clone(),
                    target: main_body,
                    fallthrough: follow_block,
                },
            );
            return follow_block;
        }
        cfg.delete(main_body);

        cfg.block_data_mut(header_block).header.drain(..);

        for Mutation { ident, old, new } in invalid_mutations {
            assert!(!created_phis.contains_key(&ident));
            let created_phi =
                cfg.add_header_statement(header_block, HeaderStatementKind::Phi(old, new));
            debug!("Created {:?} for '{:?}'", created_phi, ident);
            cfg.set_symbol(header_block, ident.clone(), created_phi);
            created_phis.insert(ident.clone(), created_phi);
        }
        info!("Created phis: {:?}", created_phis);
    }
}

pub(crate) fn lower_block(cfg: &mut ControlFlowGraph, mut blk: BlockId, block: &Block) -> BlockId {
    for statement in &block.0 {
        // FIXME deal with this clone
        blk = lower_statement(cfg, blk, statement.clone());
    }
    blk
}
