use std::collections::HashMap;

use log::{debug, info, warn};

use crate::parser::{Block, Designator, Expression, Function, Relation, Statement};

use super::{cfg::ControlFlowGraph, types::*};

// Expression cannot lower across multiple blocks
fn lower_expression(
    cfg: &mut ControlFlowGraph,
    block: BlockId,
    expr: &Expression,
) -> InstructionId {
    match expr {
        Expression::Constant(c) => cfg.get_constant(*c),
        // a lookup of an uninitialized variable returns the ZERO constant
        Expression::Identifier(ident) => cfg.resolve_symbol(block, ident),
        Expression::ArrayAccess(ident, offset) => {
            let base = cfg.resolve_symbol(block, ident);
            let offset_val = lower_expression(cfg, block, &*offset);

            let pointer = cfg.add_instruction(
                block,
                InstructionKind::BasicOp(BasicOpKind::Addi, base, offset_val),
            );
            cfg.add_instruction(block, InstructionKind::Load(Load { base, pointer }))
        }
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
                let arguments = call
                    .arguments
                    .iter()
                    .map(|expr| lower_expression(cfg, block, expr))
                    .collect();
                cfg.add_instruction(
                    block,
                    InstructionKind::Call(call.function_name.clone(), arguments),
                )
            }
        }
    }
}

fn lower_relation(cfg: &mut ControlFlowGraph, block: BlockId, relation: &Relation) -> Comparison {
    // TODO make this a little less fucked
    match relation.compare_op {
        crate::tokenizer::Token::LessThan => {
            let expr = Expression::Subtract(
                Box::new(relation.left.clone()),
                Box::new(relation.right.clone()),
            );
            let expr = lower_expression(cfg, block, &expr);

            Comparison {
                kind: ComparisonKind::LtZero,
                value: expr,
            }
        }
        crate::tokenizer::Token::GreaterThan => {
            let expr = Expression::Subtract(
                Box::new(relation.left.clone()),
                Box::new(relation.right.clone()),
            );
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
        Statement::Assign(Designator::ArrayIndex(ref ident, ref offset), expr) => {
            let value = lower_expression(cfg, block, &expr);
            let base = cfg.resolve_symbol(block, &ident);
            let offset = lower_expression(cfg, block, &offset);
            let pointer = cfg.add_instruction(
                block,
                InstructionKind::BasicOp(BasicOpKind::Addi, base, offset),
            );
            assert!(
                matches!(
                    cfg.get_instruction(base).and_then(|it| Some(it.kind)),
                    Some(InstructionKind::Constant(_))
                ) || matches!(
                    cfg.get_header(base).and_then(|it| Some(it.kind)),
                    Some(HeaderStatementKind::Param(_))
                )
            );
            cfg.add_instruction(
                block,
                InstructionKind::Store(Store {
                    base,
                    pointer,
                    value,
                }),
            );
            block
        }
        Statement::If {
            ref condition,
            ref body,
            ref else_body,
        } => lower_if(cfg, &statement, block, condition, body, else_body),
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
                let args = call
                    .arguments
                    .iter()
                    .map(|it| lower_expression(cfg, block, it))
                    .collect();
                cfg.add_instruction(block, InstructionKind::Call(call.function_name, args));
                block
            }
        }
        Statement::VoidReturn => {
            cfg.add_instruction(block, InstructionKind::Return(None));
            block
        }
        Statement::Return(expr) => {
            let instruction_id = lower_expression(cfg, block, &expr);
            cfg.add_instruction(block, InstructionKind::Return(Some(instruction_id)));
            block
        }
    }
}

fn lower_if(
    cfg: &mut ControlFlowGraph,
    statement: &Statement,
    block: BlockId,
    condition: &Relation,
    body: &Block,
    else_body: &Option<Block>,
) -> BlockId {
    debug!("Lowering statement {:?}", statement.clone());
    let header_block = block;
    let condition = lower_relation(cfg, header_block, condition);

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

fn try_lower_while_body(
    cfg: &mut ControlFlowGraph,
    header_block: BlockId,
    body: &Block,
) -> (BlockId, BlockId, Vec<Mutation>, Vec<Mutation>) {
    let old_symbols = &cfg.get_symbols(header_block).clone();

    let main_body = cfg.new_block(header_block);
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

    info!("Existing phis: {:?}", phis);
    for mutation in changed_symbols {
        if phis.contains(&mutation.old) {
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

    let mut created_phis = HashMap::new();
    let mut i = 0;
    loop {
        if i == 2 {
            panic!("While lowering was attempted too many times");
        }
        i += 1;

        info!("Try lowering while body (iteration {})", i);
        let condition = lower_relation(cfg, header_block, &condition);
        let (main_body, main_body_end, valid_mutations, invalid_mutations) =
            try_lower_while_body(cfg, header_block, &body);

        warn!("Detected invalid mutations: {:?}", invalid_mutations);

        if invalid_mutations.is_empty() {
            // fix any previously generated phi statements
            for mutation in valid_mutations {
                if let Some(instruction_id) = created_phis.get(&mutation.ident) {
                    cfg.block_data_mut(header_block)
                        .update_phi(*instruction_id, mutation.new);
                }
            }

            cfg.goto(main_body_end, header_block);
            let follow_block = cfg.new_block(header_block);
            cfg.set_terminator(
                header_block,
                Terminator::ConditionalBranch {
                    condition: condition.clone(),
                    target: main_body,
                    fallthrough: follow_block,
                },
            );
            cfg.propagate_dominance_to(root_block, follow_block);
            return follow_block;
        }
        cfg.delete(main_body);
        cfg.block_data_mut(header_block).reset();
        // restore dominance info we lost from the reset earlier
        // maybe checkpoints weren't such a bad idea after all?
        for block in cfg.predeccessors(header_block) {
            cfg.propagate_dominance_to(block, header_block);
            cfg.propagate_symbols(block, header_block);
        }

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

pub fn lower_function(function: &Function) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    let block = cfg.start_block();
    for var in &function.variables {
        let id = cfg.add_header_statement(block, HeaderStatementKind::Param(var.ident()));
        cfg.set_symbol(block, var.ident(), id);
    }
    let func_start = cfg.new_block(block);
    cfg.goto(block, func_start);
    lower_block(&mut cfg, func_start, &function.body);

    cfg
}