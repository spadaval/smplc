use log::{debug, info};
use std::collections::HashMap;

use crate::tokenizer::{Token, Tokenizer};

pub struct Parser {
    curr: Option<Token>,
    tokens: Tokenizer,
    // TODO move this somewhere else
    variables: HashMap<String, f32>,
}

impl Parser {
    pub fn new(mut tokens: Tokenizer) -> Self {
        Parser {
            curr: tokens.next(),
            tokens,
            variables: Default::default(),
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

#[derive(new)]
pub struct Computation {}

#[derive(new)]
pub struct Assignment;
#[derive(new)]
pub struct Expression;
#[derive(new)]
struct Term;
#[derive(new)]
struct Factor;

#[derive(Debug)]
pub enum ParseNode {
    Assign(String, f32),
    Number(f32),
}
use ParseNode::*;
pub type ParseResult = Result<ParseNode, ParseError>;

pub trait Parse {
    fn parse(&mut self, parser: &mut Parser) -> ParseResult;
}

impl Computation {
    fn do_parse(&mut self, parser: &mut Parser) -> ParseResult {
        match parser.curr.clone() {
            Some(Token::Identifier(s)) if s=="var" => {
                debug!("Trying to parse assignment");
                parser.advance();
                let assignment = Assignment::new().parse(parser)?;
                match assignment {
                    ParseNode::Assign(ident, num) => {
                        parser.variables.insert(ident.clone(), num);
                        info!("Assigned {} <- {}", ident, num);
                        return Ok(Number(num));
                    }
                    _ => Err(ParseError {
                        message: "wtf".to_string(),
                    }),
                }
            }
            _ => {
                let expr = Expression::new().parse(parser)?;
                print!("Computation results: {:?} ", expr);

                loop {
                    match parser.curr.clone() {
                        Some(Token::Period) => {
                            parser.advance();
                            let expr = Expression::new().parse(parser)?;
                            print!(" {:?} ", expr);
                        }
                        _ => return Ok(Number(0.0)),
                    }
                }
            }
        }
    }
}

impl Parse for Computation {
    fn parse(&mut self, parser: &mut Parser) -> ParseResult {
        loop {
            self.do_parse(parser)?;
            match parser.curr {
                Some(Token::Period) => parser.advance(),
                Some(_) => {
                    return Err(ParseError {
                        message: "Unexpected extra characters".to_string(),
                    })
                }
                None => return Ok(Number(0.0)),
            }
        }
    }
}

impl Parse for Assignment {
    fn parse(&mut self, parser: &mut Parser) -> ParseResult {
        let lvalue = parser.expect(|token| matches!(token, Token::Identifier(_)))?;

        parser.expect(|token| matches!(token, Token::Assignment))?;

        debug!("lvalue: {:?} ", lvalue);
        let expr = Expression::new().parse(parser)?;
        let ident = if let Token::Identifier(ident) = lvalue {
            ident
        } else {
            return Err(ParseError {
                message: "wtf".to_string(),
            });
        };

        match expr {
            Number(n) => Ok(ParseNode::Assign(ident, n)),
            _ => Err(ParseError {
                message: "wtf".to_string(),
            }),
        }
    }
}

impl Parse for Expression {
    fn parse(&mut self, parser: &mut Parser) -> ParseResult {
        let mut acc = 0.0;

        let term = Term::new().parse(parser)?;

        match term {
            ParseNode::Number(n) => acc += n,
            _ => {
                return Err(ParseError {
                    message: "Expected number while parsing expression".to_string(),
                })
            }
        }

        debug!("Expression: current acc={}", acc);

        loop {
            debug!("Expression: current acc={}", acc);
            match parser.curr.clone() {
                Some(t @ Token::Add) | Some(t @ Token::Subtract) => {
                    parser.advance();
                    let term2 = match Term::new().parse(parser)? {
                        Number(n) => n,
                        _ => {
                            return Err(ParseError {
                                message: "Expected number when parsing expression".to_string(),
                            })
                        }
                    };
                    match t {
                        Token::Add => acc += term2,
                        Token::Subtract => acc -= term2,
                        _ => unreachable!(),
                    }
                }
                _ => return Ok(Number(acc)),
            }
        }
    }
}

impl Parse for Term {
    fn parse(&mut self, parser: &mut Parser) -> ParseResult {
        let mut acc = 0.0;

        let factor = match Factor::new().parse(parser)? {
            Number(n) => n,
            _ => {
                return Err(ParseError {
                    message: "Expected number when parsing term".to_string(),
                })
            }
        };
        acc += factor;

        loop {
            info!("Term: got {:?}", parser.curr);
            match parser.curr.clone() {
                Some(t @ Token::Mult) | Some(t @ Token::Divide) => {
                    parser.advance();
                    let factor2 = match Factor::new().parse(parser)? {
                        Number(n) => n,
                        _ => {
                            return Err(ParseError {
                                message: "Expected number when parsing term".to_string(),
                            })
                        }
                    };
                    match t {
                        Token::Mult => acc *= factor2,
                        Token::Divide => acc /= factor2,
                        _ => unreachable!(),
                    }
                }
                _ => return Ok(Number(acc)),
            }
        }
    }
}

impl Parse for Factor {
    fn parse(&mut self, parser: &mut Parser) -> ParseResult {
        match parser.curr.clone() {
            Some(Token::Number(x)) => {
                parser.advance();
                Ok(Number(x))
            }
            Some(Token::Identifier(x)) => {
                parser.advance();
                if parser.variables.contains_key(&x) {
                    Ok(Number(*parser.variables.get(&x).unwrap()))
                } else {
                    Err(ParseError {
                        message: format!("Undefined variable {}", x),
                    })
                }
            }
            Some(Token::LeftParen) => {
                parser.advance();
                let expr = Expression::new().parse(parser);
                parser.expect(|t| matches!(t, Token::RightParen))?;
                expr
            }
            _ => Err(ParseError {
                message: "failed to parse factor".to_string(),
            }),
        }
    }
}
