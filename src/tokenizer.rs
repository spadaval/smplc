use std::{fmt::Display, rc::Rc};

use log::{info, debug, error};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    String(std::string::String),
    Number(f32),
    Identifier(String),
    Add,
    Mult,
    Subtract,
    Divide,
    LeftParen,
    RightParen,
    Period,
}

pub struct TokenizerError {
    message: String,
}
impl Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub struct ConsumeResponse {
    token: Option<Token>,
    transition: Option<Box<dyn State>>,
}
pub trait State {
    fn accept(&mut self, curr: char, next: Option<char>)
        -> Result<ConsumeResponse, TokenizerError>;
}

fn emit_and_transition(
    token: Token,
    transition: Box<dyn State>,
) -> Result<ConsumeResponse, TokenizerError> {
    Ok(ConsumeResponse {
        token: Some(token),
        transition: Some(transition),
    })
}

fn emit(token: Token) -> Result<ConsumeResponse, TokenizerError> {
    Ok(ConsumeResponse {
        token: Some(token),
        transition: None,
    })
}

fn transition(state: Box<dyn State>) -> Result<ConsumeResponse, TokenizerError> {
    Ok(ConsumeResponse {
        token: None,
        transition: Some(state),
    })
}

fn pass() -> Result<ConsumeResponse, TokenizerError> {
    Ok(ConsumeResponse {
        token: None,
        transition: None,
    })
}

pub struct Tokenizer {
    curr: Option<char>,
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
            let next = self.characters.next();

            self.curr = next;

            let res = self.state.accept(curr, next);
            match res {
                Err(err) => {
                    error!("{}", err);
                    panic!();
                }
                Ok(response) => {
                    if let Some(new_state) = response.transition {
                        self.state = new_state;
                    }
                    if response.token.is_some() {
                        return response.token;
                    }
                }
            }
        }
    }
}

struct Waiting;

impl State for Waiting {
    fn accept(
        &mut self,
        curr: char,
        _next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        match curr {
            d @ '0'..='9' => transition(Number::new(d)),
            w @ 'a'..='z' => transition(Identifier::new(w)),
            '+' => emit(Token::Add),
            '-' => emit(Token::Subtract),
            '*' => emit(Token::Mult),
            '/' => emit(Token::Divide),
            '(' => emit(Token::LeftParen),
            ')' => emit(Token::RightParen),
            '.' => emit(Token::Period),
            ' ' => pass(),
            _ => Err(TokenizerError {
                message: format!("Recieved unknown character {} while in Waiting", curr),
            }),
        }
    }
}

struct Identifier {
    acc: Vec<char>,
}
impl Identifier {
    fn new(d: char) -> Box<dyn State> {
        let mut acc = Vec::new();
        acc.push(d);
        return Box::new(Identifier { acc });
    }
}

impl State for Identifier {
    fn accept(
        &mut self,
        curr: char,
        next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        self.acc.push(curr);

        if next.is_none() || !next.unwrap().is_alphanumeric() {
            emit_and_transition(
                Token::Identifier(self.acc.drain(..).collect::<String>()),
                Box::new(Waiting {}),
            )
        } else {
            pass()
        }
    }
}

struct Number {
    acc: Vec<char>,
}
impl Number {
    fn new(d: char) -> Box<dyn State> {
        let mut acc = Vec::new();
        acc.push(d);
        return Box::new(Number { acc });
    }
}

impl State for Number {
    fn accept(
        &mut self,
        curr: char,
        next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        // TODO fix this ugly mess
        if curr.is_alphanumeric() {
            self.acc.push(curr);
        }
        info!("acc: '{:?}', curr: {}, next: {:?}", self.acc, curr, next);

        if !curr.is_alphanumeric() || next.is_none() || !next.unwrap().is_alphanumeric() {
            let str = self.acc.drain(..).collect::<String>();
            let number = str
                .parse::<f32>()
                .expect(format!("tried to parse '{}' to f32", str).as_str());
            emit_and_transition(Token::Number(number), Box::new(Waiting {}))
        } else {
            pass()
        }
    }
}
