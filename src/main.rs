use std::{env, error::Error, fmt::Display, process};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

pub mod ast;

fn main() {
    let file_path = read_args().unwrap_or_else(|err| {
        println!("Problem parsing arguments: {err}");
        usage_tip();
        process::exit(1);
    });

    println!("{file_path}");
}

#[derive(Debug)]
struct ArgsError {
    msg: String,
}

impl Error for ArgsError {}
impl Display for ArgsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

fn read_args() -> Result<String, ArgsError> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err(ArgsError { msg: "Too few arguments".to_string() })
    }

    Ok(args[1].clone())
}

fn usage_tip() {
    println!("Usage: cargo run -- <Filename.pas>");
}
