use std::rc::Rc;

use super::tokenizer::{Ident, Token, Tokenizer};
use log::{error, info};

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

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match self.curr.clone() {
            Some(Token::Identifier(ident)) => {
                self.advance();
                Ok(ident)
            }
            _ => ParseError::unexpected_token(self.curr.clone()),
        }
    }

    fn advance(&mut self) -> Option<Token> {
        let token_to_return = self.curr.clone();
        self.curr = self.tokens.next();
        token_to_return
    }
}
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}
impl ParseError {
    fn unexpected_token<T>(curr: Option<Token>) -> Result<T, ParseError> {
        Err(ParseError {
            message: format!("Recieved unexpected token {:?}", curr),
        })
    }

    fn new<T>(message: &str) -> Result<T, ParseError> {
        Err(ParseError {
            message: message.to_string(),
        })
    }
}

pub struct BlockParser;
struct StatementParser;
struct CallParser;
pub struct ExpressionParser;
pub struct RelationParser;
struct TermParser;
struct FactorParser;
struct FunctionParser;
struct LValue;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub function_name: Ident,
    pub arguments: Vec<Expression>,
}

// note: this enum's discriminant is used as the key for the dominance lookup table
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Identifier(Ident),
    ArrayAccess(Ident, Vec<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Call(FunctionCall),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, PartialEq, Clone)]
pub enum Designator {
    Ident(Ident),
    ArrayIndex(Ident, Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Relation {
    pub left: Expression,
    pub compare_op: Token,
    pub right: Expression,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum VariableType {
    I32,
    F32,
    Array(Box<VariableType>, Vec<usize>),
}
impl VariableType {
    pub(crate) fn size(&self) -> usize {
        return 4;
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Variable {
    pub ident: Ident,
    pub dtype: VariableType,
}
impl Variable {
    pub fn i32(ident: Ident) -> Variable {
        Variable {
            ident,
            dtype: VariableType::I32,
        }
    }

    pub fn array(name: Ident, shape: Vec<usize>) -> Variable {
        Variable {
            ident: name,
            dtype: VariableType::Array(Box::new(VariableType::I32), shape),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Ident,
    pub variables: Vec<Variable>,
    pub body: Block,
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
    Call(FunctionCall),
    VoidReturn,
    Return(Expression),
}

#[derive(Debug)]
pub struct Program {
    pub globals: Vec<Variable>,
    pub functions: Vec<Function>,
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
                let mut offsets = Vec::new();
                while parser.curr == Some(Token::LeftSquareBracket) {
                    parser.advance();
                    offsets.push(ExpressionParser::parse(parser)?);
                    parser.expect(|it| *it == Token::RightSquareBracket)?;
                }
                if offsets.is_empty() {
                    Ok(Designator::Ident(ident))
                } else {
                    Ok(Designator::ArrayIndex(ident, offsets))
                }
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
            | Token::Equal
            | Token::LessThanEqual => {}
            _ => panic!("Invalid compare operation"),
        }

        let right = ExpressionParser::parse(parser).unwrap();
        Ok(Relation {
            left,
            compare_op,
            right,
        })
    }
}

impl Parse for CallParser {
    type Item = FunctionCall;

    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        let function_name = match parser.curr.clone() {
            Some(Token::Identifier(ident)) => ident,
            _ => return err("Was expecting a function name"),
        };
        parser.advance();
        parser.expect(|it| matches!(it, Token::LeftParen))?;
        let mut arguments = Vec::new();
        loop {
            match parser.curr {
                Some(Token::RightParen) => {
                    parser.advance();
                    return Ok(FunctionCall {
                        function_name,
                        arguments,
                    });
                }
                _ => {
                    arguments.push(ExpressionParser::parse(parser)?);
                    if parser.curr == Some(Token::Comma) {
                        parser.advance();
                    }
                }
            }
        }
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
            Some(Token::Call) => {
                parser.advance();
                let function_call = CallParser::parse(parser)?;
                Ok(Statement::Call(function_call))
            }
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
            Some(Token::Return) => {
                parser.advance();
                if parser.curr == Some(Token::Semicolon) {
                    Ok(Statement::VoidReturn)
                } else {
                    let ret_expr = ExpressionParser::parse(parser)?;
                    Ok(Statement::Return(ret_expr))
                }
            }
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
        if parser.curr == Some(Token::Call) {
            parser.advance();
            return Ok(Expression::Call(CallParser::parse(parser)?));
        }

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

struct SubFactor;

impl Parse for SubFactor {
    type Item = Expression;

    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        match parser.curr.clone() {
            Some(Token::Number(number)) => {
                parser.advance();
                Ok(Expression::Constant(number))
            }
            Some(Token::Identifier(ident)) => {
                parser.advance();
                let mut offsets = Vec::new();

                while parser.curr == Some(Token::LeftSquareBracket) {
                    parser.advance();
                    let offset = ExpressionParser::parse(parser)?;
                    offsets.push(offset);
                    parser.expect(|it| *it == Token::RightSquareBracket)?;
                }

                if offsets.is_empty() {
                    Ok(Expression::Identifier(ident))
                } else {
                    Ok(Expression::ArrayAccess(ident, offsets))
                }
            }
            _ => ParseError::unexpected_token(parser.curr.clone()),
        }
    }
}

impl Parse for FactorParser {
    type Item = Expression;
    fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        match parser.curr.clone() {
            Some(Token::Minus) => {
                parser.advance();
                Ok(Expression::Subtract(
                    Box::new(Expression::Constant(0)),
                    Box::new(SubFactor::parse(parser)?),
                ))
            }
            Some(Token::LeftParen) => {
                parser.advance();
                let expr = ExpressionParser::parse(parser);
                parser.expect(|t| matches!(t, Token::RightParen))?;
                expr
            }
            _ => SubFactor::parse(parser),
        }
    }
}

impl Parse for FunctionParser {
    type Item = Function;

    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        let (func_name, variables) = get_func_name_and_params(parser)?;

        parser
            .expect(|it| matches!(it, Token::LeftCurlyBracket))
            .unwrap();

        let block = BlockParser::parse(parser)?;

        parser
            .expect(|it| matches!(it, Token::RightCurlyBracket))
            .unwrap();

        Ok(Function {
            name: func_name,
            variables,
            body: block,
        })
    }
}

fn get_func_name_and_params(parser: &mut Parser) -> Result<(Ident, Vec<Variable>), ParseError> {
    // The main function is allowed to be unnamed
    if parser.curr == Some(Token::LeftCurlyBracket) {
        return Ok((Ident("main".to_owned()), Vec::new()));
    }

    parser.expect(|it| *it == Token::Function)?;
    let func_name = parser.expect_ident()?;

    parser.expect(|it| *it == Token::LeftParen)?;
    let mut vars: Vec<Variable> = Vec::new();

    loop {
        let ident = parser.expect_ident()?;
        let dtype = if parser.curr == Some(Token::Colon) {
            parser.advance();
            match parser.advance() {
                Some(Token::I32) => VariableType::I32,
                Some(Token::F32) => VariableType::F32,
                Some(Token::LeftSquareBracket) => {
                    let dtype = match parser.advance() {
                        Some(Token::I32) => VariableType::I32,
                        Some(Token::F32) => VariableType::F32,
                        _ => panic!("Failed to parse array type annotation"),
                    };

                    parser.expect(|it| *it == Token::Semicolon)?;

                    let mut shape = Vec::new();

                    loop {
                        if let Some(Token::Number(num)) = parser.curr {
                            parser.advance();
                            shape.push(num as usize);
                        } else {
                            break;
                        }
                    }
                    parser.expect(|it| *it == Token::RightSquareBracket)?;

                    VariableType::Array(Box::new(dtype), shape)
                }
                _ => panic!("Failed to parse type annotation"),
            }
        } else {
            VariableType::I32
        };
        vars.push(Variable {
            ident: ident,
            dtype,
        });
        match parser.advance() {
            Some(Token::Comma) => {}
            Some(Token::RightParen) => {
                break;
            }
            t => return ParseError::unexpected_token(t),
        };
    }

    Ok((func_name, vars))
}

impl Parse for Program {
    type Item = Program;

    fn parse(parser: &mut Parser) -> Result<Self::Item, ParseError> {
        let mut globals = Vec::new();
        let mut functions = Vec::new();
        parser.expect(|it| *it == Token::Main)?;

        error!("Got main");
        loop {
            match parser.curr {
                Some(Token::Var) => {
                    parser.advance();
                    loop {
                        globals.push(Variable::i32(parser.expect_ident()?));
                        match parser.curr.clone() {
                            Some(Token::Comma) => {
                                parser.advance();
                            }
                            Some(Token::Semicolon) => {
                                parser.advance();
                                break;
                            }
                            _ => return ParseError::unexpected_token(parser.curr.clone()),
                        }
                    }
                }
                Some(Token::Array) => {
                    info!("Parsing array");
                    parser.advance();

                    let mut shape = Vec::new();
                    loop {
                        if parser.curr != Some(Token::LeftSquareBracket) {
                            break;
                        }
                        parser.advance();
                        let Some(Token::Number(size)) = parser.advance() else { return ParseError::unexpected_token(parser.curr.clone()) };
                        shape.push(size as usize);
                        parser.expect(|it| *it == Token::RightSquareBracket)?;
                    }
                    if shape.is_empty() {
                        return ParseError::new("wtf");
                    }

                    let name = parser.expect_ident()?;
                    globals.push(Variable::array(name, shape));
                    parser.expect(|it| *it == Token::Semicolon)?;
                }
                Some(Token::LeftCurlyBracket) => break,
                _ => return ParseError::unexpected_token(parser.curr.clone()),
            };
        }
        info!("Done parsing globals");
        loop {
            if parser.curr == Some(Token::Period) || parser.curr == None {
                break;
            }
            functions.push(FunctionParser::parse(parser)?);
        }

        Ok(Program { globals, functions })
    }
}

pub fn parse(mut program: SourceFile) -> Program {
    let mut parser = Parser::new(program.tokens());

    Program::parse(&mut parser).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceFile;
    use pretty_assertions::assert_eq;

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

        let expected_program = Add(Box::new(Constant(1)), Box::new(Constant(2)));
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
            Add(Box::new(Constant(1)), Box::new(Constant(2))),
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
                right: Expression::Constant(1),
            },
            body: Block(vec![
                Statement::Assign(
                    Designator::Ident(Ident("x".to_string())),
                    Expression::Constant(10),
                ),
                Statement::Assign(
                    Designator::Ident(Ident("a".to_string())),
                    Expression::Add(
                        Box::new(Expression::Identifier(Ident("a".to_string()))),
                        Box::new(Expression::Constant(1)),
                    ),
                ),
            ]),
            else_body: Some(Block(vec![Statement::Assign(
                Designator::Ident(Ident("x".to_string())),
                Expression::Constant(2),
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
                right: Expression::Constant(1),
            },
            body: Block(vec![
                Statement::Assign(
                    Designator::Ident(Ident("x".to_string())),
                    Expression::Constant(10),
                ),
                Statement::Assign(
                    Designator::Ident(Ident("a".to_string())),
                    Expression::Add(
                        Box::new(Expression::Identifier(Ident("a".to_string()))),
                        Box::new(Expression::Constant(1)),
                    ),
                ),
            ]),
        };
        assert_eq!(expected_program, expr);
    }

    #[test]
    fn test_multiline() {
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
    #[test]
    fn test_parser_functions() {
        let program = r"
            let a <- call InputNum();        
            call OutputNum(c);
            ";
        let forest = parse(crate::SourceFile::new(program));
        println!("{forest:#?}");
    }
}
