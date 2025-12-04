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
    use crate::ast::{Expr, Header, Opcode};
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

        assert_eq!(
            parser.parse("program Fibonacci").unwrap(),
            Header::Identifier(String::from("Fibonacci"))
        );
    }

    #[test]
    fn invalid_header_grammar() {
        let parser = grammar::HeaderParser::new();

        let result = parser.parse("program program");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(format!("{:?}", err).contains("program"));

        let result = parser.parse("programsomething");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(format!("{:?}", err).contains("programsomething"));
    }

    #[test]
    fn comments() {
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

        let invalid_1 = parser.parse("program Fib1 (*\n");
        let invalid_2 = parser.parse("program Fib2 {\n");

        assert!(invalid_1.is_err());
        let err = invalid_1.unwrap_err();
        assert!(format!("{:?}", err).contains("("));

        assert!(invalid_2.is_err());
        let err = invalid_2.unwrap_err();
        assert!(format!("{:?}", err).contains("{"));
    }

    #[test]
    fn string_grammar() {
        let parser = grammar::StringParser::new();

        let valid_1 = parser.parse("''");
        let valid_2 = parser.parse("'some string'");
        // TODO: Add escaped single quotes to regex
        let valid_3 = parser.parse("'some \'escaped\' string'");

        assert_eq!(valid_1, Ok(String::from("''")));
        assert_eq!(valid_2, Ok(String::from("'some string'")));
        assert_eq!(valid_3, Ok(String::from("'some \'escaped\' string'")));
    }

    #[test]
    fn invalid_string_grammar() {
        let parser = grammar::StringParser::new();

        let input = "'some string";
        let invalid_1 = parser.parse(input);
        let invalid_2 = parser.parse("some string'");

        assert!(invalid_1.is_err());
        match invalid_1.unwrap_err() {
            ParseError::InvalidToken { location } => {
                assert_eq!(&input[location..location + 1], "'");
            }
            _ => panic!("Expected ParseError::InvalidToken"),
        };
        assert!(invalid_2.is_err());
        let err = invalid_2.unwrap_err();
        assert!(format!("{:?}", err).contains("some"));
    }

    #[test]
    fn term_grammar_literals() {
        let parser = grammar::TermParser::new();

        let integer = parser.parse("100");
        let double = parser.parse("100.100");
        let string = parser.parse("'some string'");
        let boolean = parser.parse("true");
        let identifier = parser.parse("some_identifier_9");

        assert_eq!(integer.unwrap(), Box::new(Expr::Integer(100)));
        assert_eq!(double.unwrap(), Box::new(Expr::Double(100.100)));
        assert_eq!(
            string.unwrap(),
            Box::new(Expr::String(String::from("'some string'")))
        );
        assert_eq!(boolean.unwrap(), Box::new(Expr::Boolean(true)));
        assert_eq!(
            identifier.unwrap(),
            Box::new(Expr::Identifier(String::from("some_identifier_9")))
        );
    }

    #[test]
    fn term_paren_recursion() {
        let parser = grammar::TermParser::new();

        let valid_1 = parser.parse("(100)");
        let valid_2 = parser.parse("((true))");

        assert_eq!(valid_1.unwrap(), Box::new(Expr::Integer(100)));
        assert_eq!(valid_2.unwrap(), Box::new(Expr::Boolean(true)));
    }

    #[test]
    fn invalid_term_grammar() {
        let parser = grammar::TermParser::new();

        let invalid = parser.parse("9some_identifier_9");

        assert!(invalid.is_err());
        let err = invalid.unwrap_err();
        assert!(format!("{:?}", err).contains("some_identifier_9"));
    }

    #[test]
    fn unary_grammar() {
        let parser = grammar::UnaryExprParser::new();

        let valid_1 = parser.parse("+ 5");
        let valid_2 = parser.parse("- 5");
        let valid_3 = parser.parse("not true");

        assert_eq!(
            valid_1.unwrap(),
            Box::new(Expr::Unary(Opcode::Pos, Box::new(Expr::Integer(5))))
        );
        assert_eq!(
            valid_2.unwrap(),
            Box::new(Expr::Unary(Opcode::Neg, Box::new(Expr::Integer(5))))
        );
        assert_eq!(
            valid_3.unwrap(),
            Box::new(Expr::Unary(Opcode::Not, Box::new(Expr::Boolean(true))))
        );
    }

    #[test]
    fn unary_recursion() {
        let parser = grammar::UnaryExprParser::new();

        let valid = parser.parse("- + - 5");

        assert_eq!(
            valid.unwrap(),
            Box::new(Expr::Unary(
                Opcode::Neg,
                Box::new(Expr::Unary(
                    Opcode::Pos,
                    Box::new(Expr::Unary(Opcode::Neg, Box::new(Expr::Integer(5))))
                ))
            ))
        )
    }

    #[test]
    fn invalid_unary() {
        let parser = grammar::UnaryExprParser::new();

        let invalid = parser.parse("5 + ");

        assert!(invalid.is_err());
        let err = invalid.unwrap_err();
        assert!(format!("{:?}", err).contains("+"));
    }
}
