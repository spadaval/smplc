use std::{fmt::Display, rc::Rc};

#[derive(Debug)]
enum Literal {
    String(std::string::String),
    Number(f32),
}
#[derive(Debug)]
enum Operator {
    Add,
    Mult,
    Subtract,
    Divide,
}

#[derive(Debug)]
pub enum Token {
    Literal(Literal),
    Operator(Operator),
    Identifier(String),
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
    fn consume(
        &mut self,
        curr: char,
        next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError>;
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
    curr: char,
    iter: Box<dyn Iterator<Item = char>>,
    state: Box<dyn State>,
}

impl Tokenizer {
    pub fn new(program: Rc<String>) -> Self {
        let tokens = program.chars().collect::<Vec<char>>();
        Self {
            curr: ' ',
            iter: Box::new(tokens.into_iter()),
            state: Box::new(Waiting {}),
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let curr = self.curr;
            println!("Parsing char '{}'", curr);
            let next = self.iter.next();
            
            if let Some(c) = next {
                self.curr = c;
            }

            let res = self.state.consume(curr, next);
            match res {
                Err(err) => {
                    println!("{}", err);
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

            // signal end-of-stream if the underlying character stream is over
            if next.is_none() {
                return None;
            }
        }
    }
}

struct Waiting;

impl State for Waiting {
    fn consume(
        &mut self,
        curr: char,
        _next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        use Operator::*;

        match curr {
            d @ '0'..='9' => transition(Number::new(d)),
            w @ 'a'..='z' => transition(Identifier::new(w)),
            '+' => emit(Token::Operator(Add)),
            '-' => emit(Token::Operator(Subtract)),
            '*' => emit(Token::Operator(Mult)),
            '/' => emit(Token::Operator(Divide)),
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
    fn consume(
        &mut self,
        curr: char,
        next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        self.acc.push(curr);

        if next.is_none() || !next.unwrap().is_alphanumeric() {
            emit_and_transition(
                Token::Identifier(self.acc.iter().collect::<String>()),
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
    fn consume(
        &mut self,
        curr: char,
        next: Option<char>,
    ) -> Result<ConsumeResponse, TokenizerError> {
        self.acc.push(curr);

        if next.is_none() || !next.unwrap().is_alphanumeric() {
            emit_and_transition(
                Token::Identifier(self.acc.iter().collect::<String>()),
                Box::new(Waiting {}),
            )
        } else {
            pass()
        }
    }
}
