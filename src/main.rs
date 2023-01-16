mod tokenizer;
use std::{fs::File, io::Read, rc::Rc};

use clap::{error::Error, Parser};
use tokenizer::Tokenizer;

use crate::tokenizer::Token;

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

struct Program {
    program: Rc<String>,
}

impl Program {
    fn tokenStream(&mut self) -> Tokenizer {
        Tokenizer::new(self.program.clone())
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
        program: Rc::new(input_text.clone()),
    };

    let tokens = p.tokenStream().collect::<Vec<Token>>();

    println!("Input string: {}", input_text);
    println!("tokens: {:?}", tokens);
}
