use std::fs;

use crate::{file::File, lexer::Lexer};

mod lexer;
mod file;

fn main() {
    let file = File {
        name: "test.toml".to_string(),
        contents: fs::read_to_string("./src/tests/test.toml").expect("could not find ''test.toml'"),
    };
    let mut lexer = Lexer::new(file);

    match lexer.tokenize() {
        Ok(tokens) => {
            println!("{:?}", tokens);
        },
        Err(error) => {
            println!("{}", error);
        }
    }
}
