#![feature(associated_type_defaults)]
#![feature(let_chains)]
mod parser;
mod ssa;
mod tokenizer;

pub use parser::parse;
pub use parser::SourceFile;
pub use ssa::lower_program;
pub use ssa::render_program;
pub use ssa::FunctionGraph;
pub use tokenizer::Token;
pub use tokenizer::Tokenizer;
