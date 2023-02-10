use std::rc::Rc;

use super::tokenizer::{Ident, State, Token, Tokenizer};
use log::{debug, info};
use serde::Serialize;

#[derive(Clone)]
pub struct SourceFile {
    source_code: Rc<String>,
}

impl SourceFile {
    pub fn tokens(&mut self) -> Tokenizer {
        Tokenizer::new(self.source_code.clone())
    }

    pub fn new(arg: &str) -> SourceFile {
        SourceFile {
            source_code: Rc::new(arg.to_string()),
        }
    }
}

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
    #[must_use = "Handle the failure case"]
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

    fn advance(&mut self) -> Option<Token> {
        let token_to_return = self.curr.clone();
        self.curr = self.tokens.next();
        return token_to_return;
    }
}
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

pub struct BlockParser;
struct StatementParser;
pub struct ExpressionParser;
pub struct RelationParser;
struct TermParser;
struct FactorParser;

struct LValue;

#[derive(Serialize, Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(f32),
    Identifier(Ident),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, PartialEq, Clone)]
pub enum Designator {
    Ident(Ident),
    ArrayIndex(Ident, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Relation {
    pub left: Expression,
    pub compare_op: Token,
    pub right: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assign(Designator, Expression),
    If {
        condition: Relation,
        body: Block,
        else_body: Option<Block>,
    },
    While {
        condition: Relation,
        body: Block,
    },
}

#[derive(Debug)]
pub struct ProgramForest {
    pub roots: Vec<Statement>,
}

pub trait Parse {
    type Item = Statement;
    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError>;
}

fn err<R>(message: impl Into<String>) -> Result<R, ParseError> {
    Err(ParseError {
        message: message.into(),
    })
}

impl Parse for LValue {
    type Item = Designator;

    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        match parser.curr.clone() {
            Some(Token::Identifier(ident)) => {
                parser.advance();
                return Ok(Designator::Ident(ident.clone()));
            }
            _ => err("Failed while parsing designator"),
        }
    }
}

impl Parse for RelationParser {
    type Item = Relation;

    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        let left = ExpressionParser::parse(parser).unwrap();

        let compare_op = parser.advance().unwrap();
        match compare_op {
            Token::GreaterThan
            | Token::LessThan
            | Token::GreaterThanEqual
            | Token::LessThanEqual => {}
            _ => panic!("Invalid compare operation"),
        }

        let right = ExpressionParser::parse(parser).unwrap();
        return Ok(Relation {
            left,
            compare_op,
            right,
        });
    }
}

impl Parse for StatementParser {
    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        let statement = match parser.curr.clone() {
            Some(Token::Let) => {
                parser.advance();
                let lvalue = LValue::parse(parser).unwrap();
                parser.expect(|it| matches!(it, Token::Assignment)).unwrap();
                let expr = ExpressionParser::parse(parser).unwrap();
                Ok(Statement::Assign(lvalue, expr))
            }
            Some(Token::Call) => todo!(),
            Some(Token::If) => {
                parser.advance();
                let relation = RelationParser::parse(parser).unwrap();
                parser.expect(|it| matches!(it, Token::Then)).unwrap();
                let main_body = BlockParser::parse(parser).unwrap();
                let else_body = match parser.curr {
                    Some(Token::Else) => {
                        parser.advance();
                        Some(BlockParser::parse(parser).unwrap())
                    }
                    _ => None,
                };
                parser
                    .expect(|it| matches!(it, Token::Fi))
                    .expect("Unterminated if block");

                Ok(Statement::If {
                    condition: relation,
                    body: main_body,
                    else_body,
                })
            }
            Some(Token::While) => {
                parser.advance();
                let relation = RelationParser::parse(parser).unwrap();
                parser.expect(|it| matches!(it, Token::Do)).unwrap();
                let main_body = BlockParser::parse(parser).unwrap();
                parser.expect(|it| matches!(it, Token::Od)).unwrap();

                Ok(Statement::While {
                    condition: relation,
                    body: main_body,
                })
            }
            Some(Token::Return) => todo!(),
            Some(t) => panic!("unexpected token {t:?} while parsing statement"),
            None => err("Tried parsing statement with no tokens left"),
        };
        // TODO find a less shitty way to do this
        while parser.curr == Some(Token::Semicolon) {
            parser.advance();
        }
        statement
    }
}

impl Parse for BlockParser {
    type Item = Block;
    fn parse(parser: &mut Parser) -> Result<Block, ParseError> {
        let mut statements = Vec::new();
        loop {
            let statement = StatementParser::parse(parser)?;
            statements.push(statement);
            match &parser.curr {
                Some(Token::Semicolon) => {
                    parser.advance();
                    continue;
                }
                Some(x) if x.should_end_block() => return Ok(Block(statements)),
                None => return Ok(Block(statements)),
                Some(_) => continue,
            }
        }
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

// TODO handle functions and all the other crap
pub fn parse(mut program: SourceFile) -> ProgramForest {
    let mut parser = Parser::new(program.tokens());
    let mut statements = Vec::new();
    while let Ok(statement) = StatementParser::parse(&mut parser) {
        statements.push(statement);
    }
    ProgramForest { roots: statements }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceFile;
    use pretty_assertions::assert_eq;
    use pretty_env_logger::env_logger;
    use std::rc::Rc;

    #[test]
    fn test_parse_expression() {
        let mut parser = Parser::new(
            (SourceFile {
                source_code: Rc::new("1+2".to_string()),
            })
            .tokens(),
        );
        let expr = ExpressionParser::parse(&mut parser).unwrap();

        use Expression::*;

        let expected_program = Add(Box::new(Constant(1.0)), Box::new(Constant(2.0)));
        assert_eq!(expr, expected_program);
    }

    #[test]
    fn test_assign() {
        let mut parser = Parser::new(
            (SourceFile {
                source_code: Rc::new("let a <- 1+2".to_string()),
            })
            .tokens(),
        );
        let expr = StatementParser::parse(&mut parser).unwrap();

        use Expression::*;

        let expected_program = Statement::Assign(
            Designator::Ident(Ident("a".to_string())),
            Add(Box::new(Constant(1.0)), Box::new(Constant(2.0))),
        );
        assert_eq!(expected_program, expr);
    }
    #[test]
    fn test_parse_if() {
        let program = r"
if a<1 
then 
    let x <- 10; 
    let a <- a + 1 
else 
    let x <- 2 
fi
";

        let mut parser = Parser::new(SourceFile::new(program).tokens());
        let expr = StatementParser::parse(&mut parser).unwrap();

        let expected_program = Statement::If {
            condition: Relation {
                left: Expression::Identifier(Ident("a".to_string())),
                compare_op: Token::LessThan,
                right: Expression::Constant(1.0),
            },
            body: Block(vec![
                Statement::Assign(
                    Designator::Ident(Ident("x".to_string())),
                    Expression::Constant(10.0),
                ),
                Statement::Assign(
                    Designator::Ident(Ident("a".to_string())),
                    Expression::Add(
                        Box::new(Expression::Identifier(Ident("a".to_string()))),
                        Box::new(Expression::Constant(1.0)),
                    ),
                ),
            ]),
            else_body: Some(Block(vec![Statement::Assign(
                Designator::Ident(Ident("x".to_string())),
                Expression::Constant(2.0),
            )])),
        };
        assert_eq!(expected_program, expr);
    }

    #[test]
    fn test_while() {
        let mut parser = Parser::new(
            (SourceFile {
                source_code: Rc::new("while a<1 do let x <- 10; let a <- a + 1 od".to_string()),
            })
            .tokens(),
        );
        let expr = StatementParser::parse(&mut parser).unwrap();

        let expected_program = Statement::While {
            condition: Relation {
                left: Expression::Identifier(Ident("a".to_string())),
                compare_op: Token::LessThan,
                right: Expression::Constant(1.0),
            },
            body: Block(vec![
                Statement::Assign(
                    Designator::Ident(Ident("x".to_string())),
                    Expression::Constant(10.0),
                ),
                Statement::Assign(
                    Designator::Ident(Ident("a".to_string())),
                    Expression::Add(
                        Box::new(Expression::Identifier(Ident("a".to_string()))),
                        Box::new(Expression::Constant(1.0)),
                    ),
                ),
            ]),
        };
        assert_eq!(expected_program, expr);
    }

    #[test]
    fn test_multiline() {
        let _ = pretty_env_logger::init();
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
        println!("{forest:#?}");
    }
}
