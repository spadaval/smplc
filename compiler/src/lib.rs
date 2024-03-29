#![feature(associated_type_defaults)]
#![feature(let_chains)]
#[macro_use]
extern crate enum_methods;

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

use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub fn compile(src: &str) -> String {
    let dot = render_program(src);
    dot
}
