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
            // something
            (* something else *)
            (* 
                something else 
            *)
            { 
                something else 
            }
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
    use crate::ast::Header;
    use lalrpop_util::ParseError;

    use super::*;

    #[test]
    fn read_args_happy_path() {
        let args = vec![
            "executable".to_string(),
            "-compile".to_string(),
            "Filename.pas".to_string(),
        ];
        let res = read_args(&args);

        assert_eq!(res, Ok("Filename.pas".to_string()));
    }

    #[test]
    fn read_args_with_too_few_arguments() {
        let args = vec!["executable".to_string(), "Filename.pas".to_string()];
        let res = read_args(&args);

        assert_eq!(res, Err(ArgsError::TooFewArguments));
    }

    #[test]
    fn read_args_with_invalid_flag() {
        let args = vec![
            "executable".to_string(),
            "-something".to_string(),
            "Filename.pas".to_string(),
        ];
        let res = read_args(&args);

        assert_eq!(res, Err(ArgsError::InvalidArgument));
    }

    #[test]
    fn has_valid_pas_extension() {
        let path = Path::new("File.pas");
        assert!(has_pas_extension(&path));
    }

    #[test]
    fn does_not_have_pas_extension() {
        let path = Path::new("File.txt");
        let path2 = Path::new("File");
        assert!(!has_pas_extension(&path));
        assert!(!has_pas_extension(&path2));
    }

    #[test]
    fn header_grammar() {
        let parser = grammar::HeaderParser::new();
        let valid_header = parser.parse("program Fibonacci");
        let invalid_header_1 = parser.parse("program program");
        let invalid_header_2 = parser.parse("programsomething");

        assert_eq!(
            valid_header,
            Ok(Header::Identifier("Fibonacci".to_string()))
        );
        match invalid_header_1 {
            Err(ParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                expected: _,
            }) => {
                assert_eq!(token.1, "program");
            }
            _ => panic!("Expected ParseError::UnrecognizedToken"),
        };
        match invalid_header_2 {
            Err(ParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                expected: _,
            }) => {
                assert_eq!(token.1, "programsomething");
            }
            _ => panic!("Expected ParseError::UnrecognizedToken"),
        };
    }

    #[test]
    fn valid_comments() {
        // Comments should be available everywhere in the grammar
        let parser = grammar::HeaderParser::new();

        let valid_1 = parser.parse("program Fib1 // Some comment");
        let valid_2 = parser.parse(
            "
            program Fib2 (*\n\
                some comments \n\r\
                some other comment \
            *)\
            ",
        );
        let valid_3 = parser.parse(
            "
            program Fib3 {\n\
                some comments \n\r\
                some other comment \
                }\
            ",
        );

        assert_eq!(valid_1, Ok(Header::Identifier("Fib1".to_string())));
        assert_eq!(valid_2, Ok(Header::Identifier("Fib2".to_string())));
        assert_eq!(valid_3, Ok(Header::Identifier("Fib3".to_string())));
    }

    #[test]
    fn invalid_comments() {
        // Comments should be available everywhere in the grammar
        let parser = grammar::HeaderParser::new();

        let invalid_1 = parser.parse(
            "
            program Fib1 (*\n\
                some comments \n\r\
                some other comment \
            \
            ",
        );
        let invalid_2 = parser.parse(
            "
            program Fib2 {\n\
                some comments \n\r\
                some other comment \
                \
            ",
        );

        match invalid_1 {
            Err(ParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                expected: _,
            }) => {
                assert_eq!(token.1, "(");
            }
            _ => panic!("Expected ParseError::UnrecognizedToken"),
        };
        match invalid_2 {
            Err(ParseError::InvalidToken { location, .. }) => {
                assert_eq!(location, 26);
            }
            _ => panic!("Expected ParseError::InvalidToken"),
        };
    }

    #[test]
    fn string_grammar() {
        let parser = grammar::StringParser::new();

        let valid_1 = parser.parse("''");
        let valid_2 = parser.parse("'some string'");
        // TODO: Add escaped single quotes to regex
        let valid_3 = parser.parse("'some \'escaped\' string'");

        let invalid_1 = parser.parse("'some string");
        let invalid_2 = parser.parse("some string'");

        assert_eq!(valid_1, Ok(String::from("''")));
        assert_eq!(valid_2, Ok(String::from("'some string'")));
        assert_eq!(valid_3, Ok(String::from("'some \'escaped\' string'")));

        match invalid_1 {
            Err(ParseError::InvalidToken { location, .. }) => {
                assert_eq!(location, 0);
            }
            _ => panic!("Expected ParseError::InvalidToken"),
        };
        match invalid_2 {
            Err(ParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                expected: _,
            }) => {
                assert_eq!(token.1, "some");
            }
            _ => panic!("Expected ParseError::UnrecognizedToken"),
        };
    }
}
