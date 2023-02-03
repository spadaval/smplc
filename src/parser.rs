use crate::{
    tokenizer::{Ident, Token, Tokenizer},
    Program,
};
use log::{debug, info};
use serde::Serialize;

pub struct Parser {
    curr: Option<Token>,
    tokens: Tokenizer,
}

impl Parser {
    pub fn new(mut tokens: Tokenizer) -> Self {
        Parser {
            curr: tokens.next(),
            tokens,
        }
    }

    // expect the current token to match some condition.
    // If so, advance the token stream and continue.
    // otherwise, return an error.
    fn expect(&mut self, predicate: fn(&Token) -> bool) -> Result<Token, ParseError> {
        match self.curr.clone() {
            None => Err(ParseError {
                message: "Expected token, but got None".to_string(),
            }),
            Some(token) => {
                if predicate(&token) {
                    self.advance();
                    Ok(token)
                } else {
                    Err(ParseError {
                        message: format!("Recieved unexpected token {:?}", self.curr),
                    })
                }
            }
        }
    }

    fn advance(&mut self) {
        self.curr = self.tokens.next();
    }
}
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

pub struct BlockParser;

pub struct AssignmentParser;
pub struct ExpressionParser;
struct TermParser;
struct FactorParser;

#[derive(Serialize, Debug, PartialEq)]
pub enum Expression {
    Constant(f32),
    Identifier(Ident),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
}

struct Block {}

pub enum ParseNode {
    Expression(Expression),
    Binding(Ident, Expression),
    Block(Vec<ParseNode>),
    Relation {
        left: Expression,
        compare_op: Token,
        right: Expression,
    },
    If {
        condition: Box<ParseNode>,
        body: Block,
    },
    While {
        condition: Box<ParseNode>,
        body: Block,
    },
}
pub type ParseResult = Result<ParseNode, ParseError>;

pub trait Parse {
    type Item = ParseNode;
    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError>;

    fn err(&self, message: impl Into<String>) -> ParseResult {
        Err(ParseError {
            message: message.into(),
        })
    }
}

fn err<R>(message: impl Into<String>) -> Result<R, ParseError> {
    Err(ParseError {
        message: message.into(),
    })
}
//use crate::Token::Identifier;
// impl BlockParser {
//     fn do_parse(&mut self, parser: &mut Parser) -> ParseResult {
//         match parser.curr.clone() {
//             Some(Token::Identifier(Ident(s))) if s == "var" => {
//                 debug!("Trying to parse assignment");
//                 parser.advance();
//                 let assignment = AssignmentParser::parse(parser)?;
//                 match assignment {
//                     ParseNode::Assign(ref ident, num) => {
//                         return Ok()

//                         parser.variables.insert(ident, num);
//                         info!("Assigned {} <- {}", ident, num);
//                         return Ok(num);
//                     }
//                     _ => err("wtf"),
//                 }
//             }
//             _ => {
//                 let expr = ExpressionParser::parse(parser)?;
//                 print!("Computation results: {:?} ", expr);

//                 loop {
//                     match parser.curr.clone() {
//                         Some(Token::Period) => {
//                             parser.advance();
//                             let expr = ExpressionParser::parse(parser)?;
//                             print!(" {:?} ", expr);
//                         }
//                         _ => return Ok(Number(0.0)),
//                     }
//                 }
//             }
//         }
//     }
// }

// impl Parse for BlockParser {
//     fn parse(parser: &mut Parser) -> ParseResult {
//         loop {
//             BlockParser.do_parse(parser)?;
//             match parser.curr {
//                 Some(Token::Period) => parser.advance(),
//                 Some(t) => return err(format!("Unexpected extra characters after block: {:?}", t)),
//                 None => return Ok(Number(0.0)),
//             }
//         }
//     }
// }

impl Parse for AssignmentParser {
    fn parse(parser: &mut Parser) -> ParseResult {
        let lvalue = parser.expect(|token| matches!(token, Token::Identifier(_)))?;

        parser.expect(|token| matches!(token, Token::Assignment))?;

        debug!("lvalue: {:?} ", lvalue);
        let expr = ExpressionParser::parse(parser)?;
        let ident = if let Token::Identifier(ident) = lvalue {
            ident
        } else {
            return err("wtf");
        };

        Ok(ParseNode::Binding(ident, expr))
    }
}

impl Parse for ExpressionParser {
    type Item = Expression;
    fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        let mut term = Box::new(TermParser::parse(parser).unwrap());

        loop {
            match parser.curr.clone() {
                Some(t @ Token::Plus) | Some(t @ Token::Minus) => {
                    parser.advance();
                    let term2 = Box::new(TermParser::parse(parser).unwrap());

                    match t {
                        Token::Plus => term = Box::new(Expression::Add(term, term2)),
                        Token::Minus => term = Box::new(Expression::Subtract(term, term2)),
                        _ => unreachable!(),
                    }
                }
                _ => return Ok(*term),
            }
        }
    }
}

impl Parse for TermParser {
    type Item = Expression;
    fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        let mut factor = Box::new(FactorParser::parse(parser).unwrap());

        loop {
            info!("Term: got {:?}", parser.curr);
            match parser.curr.clone() {
                Some(t @ Token::Star) | Some(t @ Token::Divide) => {
                    parser.advance();
                    let factor2 = Box::new(FactorParser::parse(parser).unwrap());
                    match t {
                        Token::Star => factor = Box::new(Expression::Multiply(factor, factor2)),
                        Token::Divide => factor = Box::new(Expression::Divide(factor, factor2)),
                        _ => unreachable!(),
                    }
                }
                // TODO deal with all this box juggling nonsense
                _ => return Ok(*factor),
            }
        }
    }
}

impl Parse for FactorParser {
    type Item = Expression;
    fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        match parser.curr.clone() {
            Some(Token::Number(x)) => {
                parser.advance();
                Ok(Expression::Constant(x))
            }
            Some(Token::Identifier(x)) => {
                parser.advance();
                Ok(Expression::Identifier(x))
            }
            Some(Token::LeftParen) => {
                parser.advance();
                let expr = ExpressionParser::parse(parser);
                parser.expect(|t| matches!(t, Token::RightParen))?;
                expr
            }
            _ => err("Failed to parse factor"),
        }
    }
}

pub fn parse(mut program: Program) -> Expression {
    let mut parser = Parser::new(program.tokens());
    ExpressionParser::parse(&mut parser).expect("Failed to parse expression")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Program;
    use pretty_assertions::assert_eq;
    use std::rc::Rc;

    #[test]
    fn test_parse_expression() {
        let mut parser = Parser::new(
            (Program {
                program: Rc::new("1+2".to_string()),
            })
            .tokens(),
        );
        let expr = ExpressionParser::parse(&mut parser).unwrap();

        use Expression::*;

        let expected_program = Add(Box::new(Constant(1.0)), Box::new(Constant(2.0)));
        assert_eq!(expr, expected_program);
    }
}
