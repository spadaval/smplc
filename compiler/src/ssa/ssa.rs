use std::{collections::HashMap};

use log::{debug, info, warn, error};

use crate::{
    parser::{
        Block, Designator, Expression, Function, Program, Relation, Statement, Variable,
        VariableType,
    },
    ssa::walker::KillFinder,
    tokenizer::Ident,
};

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
        Expression::ArrayAccess(ident, offsets) => {
            let base = cfg.resolve_symbol(block, ident);
            
            let Some(dtype) = cfg.resolve_type(block, ident) else {panic!("Failed to find type for {ident:?} for array access")};

            let pointer = calculate_pointer(cfg, block, dtype, base, offsets);

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

fn is_valid_array_base(cfg: &ControlFlowGraph, ins: InstructionId) -> bool {
    let header = cfg.get_header(ins);
    warn!("Trying to use {:?} as array base", header);
    if let Some(ins) = header {
        match ins.kind {
            HeaderStatementKind::Param(var) | HeaderStatementKind::Variable(_, var) => {
                let m = matches!(var.dtype, VariableType::Array(_, _));
                if !m {
                    println!("Expected {:?} to be an array", var);
                }
                m
            }
            _ => false,
        }
    } else {
        false
    }
}

fn lower_statement(cfg: &mut ControlFlowGraph, block: BlockId, statement: Statement) -> BlockId {
    match statement {
        Statement::Assign(Designator::Ident(ident), expr) => {
            let val = lower_expression(cfg, block, &expr);
            cfg.set_symbol(
                block,
                &Variable {
                    ident,
                    dtype: VariableType::I32,
                },
                val,
            );
            block
        }
        Statement::Assign(Designator::ArrayIndex(ref ident, ref offsets), expr) => {
            let value = lower_expression(cfg, block, &expr);

            let base = cfg.resolve_symbol(block, &ident);
            let Some(dtype) = cfg.resolve_type(block, ident) else {panic!()};

            let pointer = calculate_pointer(cfg, block, dtype, base, offsets);

            assert!(is_valid_array_base(cfg, base));
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

fn calculate_pointer(
    cfg: &mut ControlFlowGraph,
    block: BlockId,
    dtype: VariableType,
    base: InstructionId,
    offsets: &Vec<Expression>,
) -> InstructionId {
    let mut offset = cfg.get_constant(0);

    let VariableType::Array(dtype, shape) = dtype else {panic!("Tried to index into a non-array")};

    let pairs: Vec<(&Expression, usize)> = offsets.into_iter().zip(shape.into_iter()).collect();

    // TODO make this not so insanely hard to read
    for (offset_expr, shape) in pairs.iter().rev() {
        let scaler = cfg.get_constant(*shape as i32);
        let scaled_current = cfg.add_instruction(
            block,
            InstructionKind::BasicOp(BasicOpKind::Multiply, offset, scaler),
        );

        let this_dim = lower_expression(cfg, block, offset_expr);

        offset = cfg.add_instruction(
            block,
            InstructionKind::BasicOp(BasicOpKind::Add, scaled_current, this_dim),
        );
    }

    let scaler = cfg.get_constant(dtype.size() as i32);
    let scaled_offset = cfg.add_instruction(
        block,
        InstructionKind::BasicOp(BasicOpKind::Multiply, offset, scaler),
    );
    let pointer = cfg.add_instruction(
        block,
        InstructionKind::BasicOp(BasicOpKind::Addi, base, scaled_offset),
    );
    pointer
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
    let kills = KillFinder::new(cfg, header_block, follow_block).collect_kills();
    for kill in kills {
        cfg.add_header_statement(follow_block, HeaderStatementKind::Kill(kill));
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

    for mutation in changed_symbols {
        if phis.contains(&mutation.old) {
            valid_mutations.push(mutation);
        } else {
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
            let kills = KillFinder::new(cfg, header_block, follow_block).collect_kills();
            for kill in kills {
                cfg.add_header_statement(follow_block, HeaderStatementKind::Kill(kill));
            }
            return follow_block;
        }

        warn!(
            "Detected invalid mutations, undoing lowering: {:?}",
            invalid_mutations
        );

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
            cfg.update_symbol(header_block, &ident, created_phi);
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

pub fn lower_function(
    function: &Function,
    globals: &Vec<Variable>,
    is_main: bool,
) -> ControlFlowGraph {
    let linkage = if is_main {
        Linkage::Global
    } else {
        Linkage::Extern
    };
    let mut cfg = ControlFlowGraph::new(globals, linkage);

    let block = cfg.start_block();
    cfg.propagate_symbols(cfg.const_block_id(), cfg.start_block());

    for var in &function.variables {
        let id = cfg.add_header_statement(block, HeaderStatementKind::Param(var.clone()));
        cfg.set_symbol(block, var, id);
        cfg.register_param(var);
    }
    let func_start = cfg.new_block(block);
    cfg.goto(block, func_start);
    lower_block(&mut cfg, func_start, &function.body);
    cfg.skip_empty_blocks();

    cfg
}

pub fn lower_program(program: Program) -> Vec<(Function, ControlFlowGraph)> {
    let main_function: Vec<&Function> = program
        .functions
        .iter()
        .filter(|it| it.name == Ident("main".to_owned()))
        .collect();
    assert!(main_function.len() == 1);
    program
        .functions
        .into_iter()
        .map(|func| {
            let cfg = lower_function(&func, &program.globals, func.name.0 == "main");
            (func, cfg)
        })
        .collect()
}
