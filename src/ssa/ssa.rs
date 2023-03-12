use std::fmt::Display;

use log::{debug, info};

use crate::parser::{Block, Designator, Expression, Relation, Statement};

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
fn lower_expression(cfg: &mut ControlFlowGraph, block: BlockId, expr: &Expression) -> InstructionId {
    let existing_expression: Option<InstructionId> = cfg.lookup_expression(block, expr);
    match existing_expression {
        Some(id) => id,
        None => {
            let id = do_lower_expression(cfg, block, expr);
            cfg.save_expression(block, expr, id);
            id
        }
    }
}

fn do_lower_expression(cfg: &mut ControlFlowGraph, block: BlockId, expr: &Expression) -> InstructionId {
    match expr {
        Expression::Constant(c) => cfg.get_constant(*c),
        // a lookup of an uninitialized variable returns the ZERO constant
        Expression::Identifier(ident) => cfg.resolve_symbol(block, ident.clone()),
        Expression::Add(l, r) => {
            let add = InstructionKind::BasicOp(BasicOpKind::Add, lower_expression(cfg, block, l), lower_expression(cfg, block, r));
            cfg.add_instruction(block, add)
        }
        Expression::Subtract(l, r) => {
            let sub = InstructionKind::BasicOp(BasicOpKind::Subtract, lower_expression(cfg, block, l), lower_expression(cfg, block, r));
            cfg.add_instruction(block, sub)
        }
        Expression::Multiply(l, r) => {
            let mul = InstructionKind::BasicOp(BasicOpKind::Multiply, lower_expression(cfg, block, l), lower_expression(cfg, block, r));
            cfg.add_instruction(block, mul)
        }
        Expression::Divide(l, r) => {
            let div = InstructionKind::BasicOp(BasicOpKind::Divide, lower_expression(cfg, block, l), lower_expression(cfg, block, r));
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
        Statement::If { ref condition, ref body, ref else_body } => {
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
                    debug!("Lowered else_body from block {} to block {}", else_body_start.0, else_body_end.0);
                    cfg.goto(main_body_end, follow_block);
                    cfg.goto(else_body_end, follow_block);
                }
                None => {
                    debug!("Lowered condition to block {}", header_block.0);
                    debug!("Add edge {} -(target)-> {}, {} -(fallthrough)-> {}", header_block.0, main_body.0, header_block.0, follow_block.0);
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
            let condition = lower_relation(cfg, header_block, condition);

            let main_body = cfg.new_block(header_block);
            let main_body_end = lower_block(cfg, main_body, &body);

            let follow_block = cfg.new_block(header_block);

            cfg.set_terminator(
                header_block,
                Terminator::ConditionalBranch {
                    condition,
                    target: main_body,
                    fallthrough: follow_block,
                },
            );
            debug!("Add edge {} -(target)-> {}, {} -(fallthrough)-> {}", header_block.0, main_body.0, header_block.0, follow_block.0);

            cfg.goto(main_body_end, header_block);

            follow_block
        }
        Statement::Call(call) => {
            if call.function_name.0 == "OutputNum" {
                let arg = call.arguments.first().expect("A call to OutputNum should have at least one argument");
                let ins = lower_expression(cfg, block, arg);
                cfg.add_instruction(block, InstructionKind::Write(ins));
                block
            } else {
                todo!()
            }
        }
    }
}

pub(crate) fn lower_block(cfg: &mut ControlFlowGraph, mut blk: BlockId, block: &Block) -> BlockId {
    for statement in &block.0 {
        // FIXME deal with this clone
        blk = lower_statement(cfg, blk, statement.clone());
    }
    blk
}
