use lalrpop_util::ParseError;
use std::{env, error::Error, fmt::Display, fs, path::Path, process};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

pub mod ast;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = read_args(&args).unwrap_or_else(|err| {
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

    println!(
        "{:?}",
        grammar::ProgramParser::new().parse(
            "
            program something \n\
            some_int : integer = 10 + 2;\n\
            some_double : double = 10 + 2;\n\
            some_bool : boolean = 10 + 2;\n\
            some_string : string = 10 + 2;\n\
            some_string:string=10+2;\n\
            "
        )
    );
}

fn has_pas_extension(path: &Path) -> bool {
    if let Some(ext) = path.extension() {
        ext == "pas"
    } else {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ArgsError {
    TooFewArguments,
    InvalidArgument,
}

impl Error for ArgsError {}
impl Display for ArgsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooFewArguments => write!(f, "Too few arguments"),
            Self::InvalidArgument => write!(f, "Invalid argument"),
        }
    }
}

fn read_args(args: &Vec<String>) -> Result<String, ArgsError> {
    if args.len() < 3 {
        return Err(ArgsError::TooFewArguments);
    }

    match args[1].as_str() {
        "-compile" => Ok(args[2].clone()),
        _ => Err(ArgsError::InvalidArgument),
    }
}

fn usage_tip() {
    println!("Usage: cargo run -- -compile <Filename.pas>");
}

#[cfg(test)]
mod test {
    use lalrpop_util::lexer::Token;

    use crate::ast::Header;

    use super::*;

    #[test]
    fn test_read_args_happy_path() {
        let args = vec![
            "executable".to_string(),
            "-compile".to_string(),
            "Filename.pas".to_string(),
        ];
        let res = read_args(&args);

        assert_eq!(res, Ok("Filename.pas".to_string()));
    }

    #[test]
    fn test_read_args_with_too_few_arguments() {
        let args = vec!["executable".to_string(), "Filename.pas".to_string()];
        let res = read_args(&args);

        assert_eq!(res, Err(ArgsError::TooFewArguments));
    }

    #[test]
    fn test_read_args_with_invalid_flag() {
        let args = vec![
            "executable".to_string(),
            "-something".to_string(),
            "Filename.pas".to_string(),
        ];
        let res = read_args(&args);

        assert_eq!(res, Err(ArgsError::InvalidArgument));
    }

    #[test]
    fn test_has_pas_extension() {
        let path = Path::new("File.pas");
        assert!(has_pas_extension(&path));
    }

    #[test]
    fn test_does_not_have_pas_extension() {
        let path = Path::new("File.txt");
        let path2 = Path::new("File");
        assert!(!has_pas_extension(&path));
        assert!(!has_pas_extension(&path2));
    }

    #[test]
    fn test_header_grammar() {
        let parser = grammar::HeaderParser::new();
        let valid_header = parser.parse("program Fibonacci");
        let invalid_header_1 = parser.parse("program program");
        let invalid_header_2 = parser.parse("programsomething");

        assert_eq!(
            valid_header,
            Ok(Box::new(Header::Identifier("Fibonacci".to_string())))
        );
        assert_eq!(
            invalid_header_1,
            Err(ParseError::UnrecognizedToken {
                token: (8, Token(16, "program"), 15),
                expected: vec!["Id".to_string()]
            })
        );
        assert_eq!(
            invalid_header_2,
            Err(ParseError::UnrecognizedToken {
                token: (0, Token(1, "programsomething"), 16),
                expected: vec!["\"program\"".to_string()]
            })
        );
    }
}
