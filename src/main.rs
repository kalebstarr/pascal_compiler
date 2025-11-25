use std::{env, error::Error, fmt::Display, fs, process};

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

    let file_content = read_file(file_path).unwrap_or_else(|err| {
        println!("Problem reading file: {err}");
        usage_tip();
        process::exit(1);
    });

    println!("{file_content}");
}

macro_rules! def_error {
    ($name:ident, $msg:expr) => {
        #[derive(Debug)]
        struct $name {
            msg: String,
        }

        impl Error for $name {}
        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}: {}", $msg, self.msg)
            }
        }
    };
}

def_error!(ArgsError, "Argument Error");
def_error!(FileError, "File Error");

fn read_args() -> Result<String, ArgsError> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        return Err(ArgsError { msg: "Too few arguments".to_string() })
    }

    match args[1].as_str() {
        "-compile" => Ok(args[2].clone()),
        _ => Err(ArgsError { msg: "Invalid argument".to_string() }),
    }
}

fn read_file(file_path: String) -> Result<String, FileError> {
    let file_content = fs::read_to_string(file_path);

    match file_content {
        Ok(content) => Ok(content.clone()),
        Err(_) => Err(FileError{ msg: "Could not read file".to_string() }),
    }
}

fn usage_tip() {
    println!("Usage: cargo run -- -compile <Filename.pas>");
}
