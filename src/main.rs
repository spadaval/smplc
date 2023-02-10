use std::{fs::File, io::Read};

use clap::error::Error;

use pretty_env_logger::env_logger;
use smplc::{parse, SourceFile, Token};

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
        Err(error) => panic!("Couldn't open the file: {error}"),
    };

    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => Ok(contents),
        Err(error) => panic!("Couldn't read the file: {error}"),
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
        unreachable!()
    };
    let p = SourceFile::new(&input_text);

    println!("Input string: {input_text}");
    let tokens = p.clone().tokens().collect::<Vec<Token>>();
    println!("tokens: {tokens:?}");

    let ast = parse(p);
    println!("{ast:#?}");
}
