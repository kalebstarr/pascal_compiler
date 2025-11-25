use std::path::Path;
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

    let path = Path::new(&file_path);
    if !has_pas_extension(&path) {
        eprintln!("Problem reading file: Invalid file extension");
        usage_tip();
        process::exit(1);
    }

    let file_content = fs::read_to_string(path).unwrap_or_else(|err| {
        println!("Problem reading file: {err}");
        usage_tip();
        process::exit(1);
    });

    println!("{file_content}");
}

fn has_pas_extension(path: &Path) -> bool {
    if let Some(ext) = path.extension() {
        ext == "pas"
    } else {
        false
    }
}

macro_rules! def_error {
    ($name:ident) => {
        #[derive(Debug)]
        struct $name {
            msg: String,
        }

        impl Error for $name {}
        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.msg)
            }
        }
    };
}

def_error!(ArgsError);

fn read_args() -> Result<String, ArgsError> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        return Err(ArgsError {
            msg: "Too few arguments".to_string(),
        });
    }

    match args[1].as_str() {
        "-compile" => Ok(args[2].clone()),
        _ => Err(ArgsError {
            msg: "Invalid argument".to_string(),
        }),
    }
}

fn usage_tip() {
    println!("Usage: cargo run -- -compile <Filename.pas>");
}
