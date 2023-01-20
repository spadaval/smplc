use log::{debug, info};

use crate::tokenizer::{Token, Tokenizer};

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
            None => {
                Err(ParseError {
                    message: "blargh".to_string(),
                })
            }
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

pub struct Computation;
pub struct Expression;
struct Term;
struct Factor;

pub type ParseResult = Result<f32, ParseError>;

pub trait Parse {
    fn parse(parser: &mut Parser) -> ParseResult;
}

impl Parse for Computation {
    fn parse(parser: &mut Parser) -> ParseResult {
        let expr = Expression::parse(parser)?;
        print!("Computation results: {} ", expr);

        loop {
            match parser.curr.clone() {
                Some(Token::Period) => {
                    parser.advance();
                    let expr = Expression::parse(parser)?;
                    print!(" {} ", expr);
                }
                _ => return Ok(0.0)
            }
        }
    }
}

impl Parse for Expression {
    fn parse(parser: &mut Parser) -> ParseResult {
        let mut acc = 0.0;

        let term = Term::parse(parser)?;
        acc += term;

        debug!("Expression: current acc={}", acc);

        loop {
            match parser.curr.clone() {
                Some(t @ Token::Add) | Some(t @ Token::Subtract) => {
                    parser.advance();
                    let term2 = Term::parse(parser)?;
                    match t {
                        Token::Add => acc += term2,
                        Token::Subtract => acc -= term2,
                        _ => unreachable!(),
                    }
                }
                _ => return Ok(acc),
            }
        }
    }
}

impl Parse for Term {
    fn parse(parser: &mut Parser) -> ParseResult {
        let mut acc = 0.0;

        let factor = Factor::parse(parser)?;
        acc += factor;

        loop {
            info!("Term: got {:?}", parser.curr);
            match parser.curr.clone() {
                Some(t @ Token::Mult) | Some(t @ Token::Divide) => {
                    parser.advance();
                    let factor2 = Factor::parse(parser)?;
                    match t {
                        Token::Mult => acc *= factor2,
                        Token::Divide => acc /= factor2,
                        _ => unreachable!(),
                    }
                }
                _ => return Ok(acc),
            }
        }
    }
}

impl Parse for Factor {
    fn parse(parser: &mut Parser) -> ParseResult {
        match parser.curr {
            Some(Token::Number(x)) => {
                parser.advance();
                Ok(x)
            }
            Some(Token::LeftParen) => {
                parser.advance();
                let expr = Expression::parse(parser);
                parser.expect(|t| matches!(t, Token::RightParen))?;
                expr
            }
            _ => Err(ParseError {
                message: "failed to parse factor".to_string(),
            }),
        }
    }
}
