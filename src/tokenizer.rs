use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use log::{debug, info};
use serde::{Deserialize, Serialize};
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize, Eq, Hash)]
pub struct Ident(pub std::string::String);

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    //String(std::string::String),
    Number(i32),
    Identifier(Ident),
    // arithmetic
    Plus,
    Star,
    Minus,
    Divide,
    // relational
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    // misc operators
    LeftParen,
    RightParen,
    Period,
    Assignment,
    // keywords
    Var,
    Let,
    Call,
    If,
    Then,
    Else,
    Fi,
    While,
    Return,
    Do,
    Od,
    // structure
    Semicolon,
    Comma,
}

impl Token {
    pub fn should_end_block(&self) -> bool {
        match self {
            Token::Fi | Token::Od | Token::Else => true,
            _ => false,
        }
    }
}

fn identifier_to_keyword(token: Token) -> Option<Token> {
    let ident = match token {
        Token::Identifier(s) => s,
        _ => panic!("Tried to convert a non-identifer to a reserved word"),
    };
    match ident.0.as_str() {
        "var" => Some(Token::Var),
        "let" => Some(Token::Let),
        "then" => Some(Token::Then),
        "call" => Some(Token::Call),
        "return" => Some(Token::Return),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "fi" => Some(Token::Fi),
        "while" => Some(Token::While),
        "do" => Some(Token::Do),
        "od" => Some(Token::Od),
        _ => None,
    }
}

pub struct TokenizerError {
    message: String,
}
impl Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

type TokenizerResult = Result<ConsumeResponse, TokenizerError>;

pub struct ConsumeResponse {
    token: Option<Token>,
    transition: Option<Box<dyn State>>,
    advance: bool,
}
pub trait State: Debug {
    fn accept(&mut self, curr: char, next: Option<char>) -> TokenizerResult;
}

fn emit_and_transition(token: Token, transition: Box<dyn State>) -> TokenizerResult {
    Ok(ConsumeResponse {
        token: Some(token),
        transition: Some(transition),
        advance: true,
    })
}

fn emit(token: Token) -> TokenizerResult {
    Ok(ConsumeResponse {
        token: Some(token),
        transition: None,
        advance: true,
    })
}

fn transition(state: Box<dyn State>) -> TokenizerResult {
    Ok(ConsumeResponse {
        token: None,
        transition: Some(state),
        advance: false,
    })
}

fn transition_and_advance(state: Box<dyn State>) -> TokenizerResult {
    Ok(ConsumeResponse {
        token: None,
        transition: Some(state),
        advance: true,
    })
}

fn pass() -> TokenizerResult {
    Ok(ConsumeResponse {
        token: None,
        transition: None,
        advance: true,
    })
}

pub struct Tokenizer {
    // end-of-stream can either be signalled by this becoming null, or by another boolean. I'm not sure yet which one is less terrible.
    curr: Option<char>,
    next: Option<char>,
    characters: Box<dyn Iterator<Item = char>>,
    state: Box<dyn State>,
}

impl Tokenizer {
    pub fn new(program: Rc<String>) -> Self {
        // TODO figure out how to make this lazy
        let tokens = program.chars().collect::<Vec<char>>();
        let mut characters = Box::new(tokens.into_iter());
        Self {
            curr: characters.next(),
            next: characters.next(),
            characters,
            state: Box::new(Waiting {}),
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            debug!("[Tokenizer] Parsing char '{:?}'", self.curr);

            let curr = self.curr?;
            let next = self.next;

            let res = self.state.accept(curr, next);
            match res {
                Err(err) => {
                    panic!("Error: {err}");
                }
                Ok(response) => {
                    if response.advance {
                        debug!("Advancing tokenstream");
                        self.curr = next;
                        self.next = self.characters.next();
                    }
                    if let Some(new_state) = response.transition {
                        debug!("Move to new state: {:?}", new_state);
                        self.state = new_state;
                    }
                    if response.token.is_some() {
                        info!("Emit {:?}", response.token);
                        return response.token;
                    }
                }
            }
        }
    }
}
#[derive(Debug)]
struct Waiting;

impl State for Waiting {
    fn accept(&mut self, curr: char, next: Option<char>) -> TokenizerResult {
        match curr {
            '0'..='9' => transition(Number::new()),
            'a'..='z' | 'A'..='Z' => transition(Ident::new()),
            '<' => match next {
                Some('-') => transition_and_advance(Assign::new()),
                Some('=') => emit(Token::LessThanEqual),
                _ => emit(Token::LessThan),
            },
            '>' => match next {
                Some('-') => transition_and_advance(Assign::new()),
                Some('=') => emit(Token::GreaterThanEqual),
                _ => emit(Token::GreaterThan),
            },
            '+' => emit(Token::Plus),
            '-' => emit(Token::Minus),
            '*' => emit(Token::Star),
            '/' => emit(Token::Divide),
            '(' => emit(Token::LeftParen),
            ')' => emit(Token::RightParen),
            '.' => emit(Token::Period),
            ';' => emit(Token::Semicolon),
            ',' => emit(Token::Comma),
            c if c.is_whitespace() => pass(),
            _ => Err(TokenizerError {
                message: format!("Recieved unknown character '{curr}' while in Waiting"),
            }),
        }
    }
}

#[derive(Debug)]
struct IdentifierState {
    acc: Vec<char>,
}
impl Ident {
    fn new() -> Box<dyn State> {
        Box::new(IdentifierState { acc: Vec::new() })
    }
}

impl State for IdentifierState {
    fn accept(&mut self, curr: char, next: Option<char>) -> TokenizerResult {
        self.acc.push(curr);

        if next.is_none() || !next.unwrap().is_alphanumeric() {
            let token = Token::Identifier(Ident(self.acc.drain(..).collect::<String>()));
            emit_and_transition(
                identifier_to_keyword(token.clone()).unwrap_or(token),
                Box::new(Waiting {}),
            )
        } else {
            pass()
        }
    }
}

#[derive(Debug)]
struct Number {
    acc: Vec<char>,
}
impl Number {
    fn new() -> Box<dyn State> {
        Box::new(Number { acc: Vec::new() })
    }
}

impl State for Number {
    fn accept(&mut self, curr: char, next: Option<char>) -> TokenizerResult {
        // TODO fix this ugly mess
        if curr.is_alphanumeric() {
            self.acc.push(curr);
        }
        info!(
            "[Number]: acc: '{:?}', curr: {}, next: {:?}",
            self.acc, curr, next
        );

        if !curr.is_alphanumeric() || next.is_none() || !next.unwrap().is_alphanumeric() {
            let str = self.acc.drain(..).collect::<String>();
            let number = str
                .parse::<i32>()
                .unwrap_or_else(|_| panic!("tried to parse '{str}' to i32"));
            emit_and_transition(Token::Number(number), Box::new(Waiting {}))
        } else {
            pass()
        }
    }
}

#[derive(Debug)]
struct Assign {}
impl Assign {
    fn new() -> Box<dyn State> {
        Box::new(Assign {})
    }
}

impl State for Assign {
    fn accept(&mut self, curr: char, _next: Option<char>) -> TokenizerResult {
        match curr {
            '-' => emit_and_transition(Token::Assignment, Box::new(Waiting {})),
            _ => Err(TokenizerError {
                message: format!("Recieved unknown character {curr} while in Assign"),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceFile;
    use pretty_assertions::assert_eq;
    use std::{iter::zip};

    #[test]
    fn test_tokenize_expression() {
        let tokens = SourceFile::new("1+2-(4+5) * a").tokens();

        let expected_tokens = vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Minus,
            Token::LeftParen,
            Token::Number(4),
            Token::Plus,
            Token::Number(5),
            Token::RightParen,
            Token::Star,
            Token::Identifier(Ident("a".to_string())),
        ];

        zip(tokens, expected_tokens).for_each(|(actual, expected)| assert_eq!(actual, expected))
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
        let tokens = SourceFile::new(program).tokens().collect::<Vec<Token>>();
        println!("{tokens:?}");
        assert_eq!(tokens.len(), 31);
    }
}
