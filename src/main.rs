use std::{fs::File, io::Read, rc::Rc};

use clap::{error::Error, Parser};

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The input file
    input_file: Option<String>,

    /// Immediate input. Should not be used with `input`
    #[arg(short, long)]
    input_text: Option<String>,
}

fn open(s: String) -> Result<String, Error> {
    let mut file = match File::open(s) {
        Ok(file) => file,
        Err(error) => panic!("Couldn't open the file: {}", error),
    };

    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => return Ok(contents),
        Err(error) => panic!("Couldn't read the file: {}", error),
    }
}

#[derive(Debug)]
enum Literal {
    //String(std::string::String),
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
enum Token {
    Literal(Literal),
    Operator(Operator),
}

struct TokenStream {
    curr: char,
    iter: Box<dyn Iterator<Item = char>>,
    state: State,
    acc: Vec<char>,
}

impl TokenStream {
    fn new(program: Rc<String>) -> Self {
        let tokens = program.chars().collect::<Vec<char>>();
        Self {
            curr: ' ',
            iter: Box::new(tokens.into_iter()),
            state: State::Waiting,
            acc: Default::default(),
        }
    }
}

enum State {
    Waiting,
    Identifier,
    Number,
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let curr = self.curr;
            let next = self.iter.next();
            // I don't really like this, but this has to happen before the returns below. To fix it, we'd need to move the returns out, probably by separating the state machine out.
            if let Some(x) = next {
                self.curr = x;
            }

            match self.state {
                State::Waiting => match self.curr {
                    ' ' => continue,
                    '0'..='9' => {
                        self.acc.push(self.curr);
                        self.state = State::Number;
                        continue;
                    }
                    'a'..='z' => {
                        self.acc.push(self.curr);
                        self.state = State::Identifier;
                        continue;
                    }
                    '+' => return Some(Token::Operator(Operator::Add)),
                    '-' => return Some(Token::Operator(Operator::Subtract)),
                    '*' => return Some(Token::Operator(Operator::Mult)),
                    '/' => return Some(Token::Operator(Operator::Divide)),
                    _ => todo!(),
                },
                State::Identifier => match curr {},
                State::Number => todo!(),
            }

        }
    }
}

struct Program {
    program: Rc<String>,
}

impl Program {
    fn tokenStream(&mut self) -> TokenStream {
        TokenStream::new(self.program.clone())
    }
}

fn main() {
    let args = Args::parse();

    let input_text = if let Some(x) = args.input_text {
        x
    } else if let Some(x) = args.input_file {
        open(x).unwrap()
    } else {
        " ".to_string()
    };
    let mut p = Program {
        program: Rc::new(input_text),
    };

    let tokens = p.tokenStream().collect::<Vec<Token>>();

    println!("Input: {:?}", tokens);
}
