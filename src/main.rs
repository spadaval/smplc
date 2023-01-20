mod parser;
mod tokenizer;
use std::{fs::File, io::Read, rc::Rc};

use clap::error::Error;
use log::error;
use tokenizer::Tokenizer;

use crate::{
    parser::{Expression, Parse, Parser},
    tokenizer::Token,
};

/// Simple program to greet a person
#[derive(clap::Parser, Debug)]
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

#[derive(Clone)]
struct Program {
    program: Rc<String>,
}

impl Program {
    fn tokens(&mut self) -> Tokenizer {
        Tokenizer::new(self.program.clone())
    }
}

fn main() {
    env_logger::init();
    let args = <Args as clap::Parser>::parse();

    let input_text = if let Some(x) = args.input_text {
        x
    } else if let Some(x) = args.input_file {
        open(x).unwrap()
    } else {
        " ".to_string()
    };
    let mut p = Program {
        program: Rc::new(input_text.clone()),
    };

    println!("Input string: {}", input_text);
    let tokens = p.clone().tokens().collect::<Vec<Token>>();
    println!("tokens: {:?}", tokens);

    let mut parser = Parser::new(p.tokens());
    match Expression::parse(&mut parser) {
        Ok(r) => println!("Result: {}", r),
        Err(e) => error!("{}", e.message),
    };
}
