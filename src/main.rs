use std::{
    io::{Read, stderr, stdin, stdout},
    path::PathBuf,
};

use clap::{Command, arg, value_parser};
use colored::Colorize;
use interpreter::{InterpretationData, interpret};
use lexer::Lexer;

pub mod interpreter;
pub mod lexer;

fn main() {
    let matches = Command::new("rccl")
        .author("Umatriz")
        .version("0.1.0")
        .about("This is an interpreter for CCL (Cool Char Lang) written in Rust")
        .arg(
            arg!(file: <FILE> "Path to CCL file to interpret").value_parser(value_parser!(PathBuf)),
        )
        .get_matches();

    if let Some(file_path) = matches.get_one::<PathBuf>("file") {
        let content = std::fs::read_to_string(file_path).expect("Failed to open file");
        let lexer = Lexer::new(&content);

        let mut data = InterpretationData::new(stdin(), stdout());
        if let Err((err, ctx)) = interpret(&mut lexer.iter_tokens(), &mut data) {
            let lines = content.lines().collect::<Vec<_>>();

            // for (n, l) in lines.enumerate() {
            //     println!("[{n}]{l}")
            // }

            let n = ctx.line;

            println!("[{}] {}: {err:?}", ctx, "Error".red());

            if let Some(prev) = lines.get(n - 1) {
                println!("{prev}");
            }
            let error = lines.get(n).unwrap();
            println!("{error}");
            println!("{:->1$}", "^".yellow(), ctx.column);

            if let Some(next) = lines.get(n + 1) {
                println!("{next}",);
            }

            println!();
        };

        println!("{data}");
    }
}
