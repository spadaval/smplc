use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use log::{debug, error, info};

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
    advance: bool,
}
pub trait State: Debug {
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
        advance: true,
    })
}

fn emit(token: Token) -> Result<ConsumeResponse, TokenizerError> {
    Ok(ConsumeResponse {
        token: Some(token),
        transition: None,
        advance: true,
    })
}

fn transition(state: Box<dyn State>) -> Result<ConsumeResponse, TokenizerError> {
    Ok(ConsumeResponse {
        token: None,
        transition: Some(state),
        advance: false,
    })
}

fn pass() -> Result<ConsumeResponse, TokenizerError> {
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
                    error!("{}", err);
                    panic!();
                }
                Ok(response) => {
                    if response.advance {
                        self.curr = next;
                        self.next = self.characters.next();
                    }
                    if let Some(new_state) = response.transition {
                        debug!("Move to new state: {:?}", new_state);
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
#[derive(Debug)]
struct Waiting;

impl State for Waiting {
    fn accept(
        &mut self,
        curr: char,
        _next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        match curr {
            '0'..='9' => transition(Number::new()),
            'a'..='z' => transition(Identifier::new()),
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

#[derive(Debug)]
struct Identifier {
    acc: Vec<char>,
}
impl Identifier {
    fn new() -> Box<dyn State> {
        Box::new(Identifier { acc: Vec::new() })
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
    fn accept(
        &mut self,
        curr: char,
        next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
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
                .parse::<f32>()
                .unwrap_or_else(|_| panic!("tried to parse '{}' to f32", str));
            emit_and_transition(Token::Number(number), Box::new(Waiting {}))
        } else {
            pass()
        }
    }
}
