use std::{env, error::Error, fmt::Display, fs, path::Path, process};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

pub mod ast;
mod type_checker;

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

    let ast = grammar::ProgramParser::new().parse(&file_content);
    println!("{:?}", ast);
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
}

#[cfg(test)]
mod test_grammar {
    use crate::ast::{
        BinaryOp, Expr, FunctionCall, FunctionDeclaration, Header, IfElse, Literal, Parameter,
        Statement, Type, UnaryOp, VariableAssignment, VariableDeclaration, While,
    };
    use lalrpop_util::ParseError;

    use super::*;

    #[test]
    fn header_() {
        let parser = grammar::HeaderParser::new();

        assert_eq!(
            parser.parse("program Fibonacci;").unwrap(),
            Header::Identifier(String::from("Fibonacci"))
        );
    }

    #[test]
    fn invalid_header() {
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

        let valid_1 = parser.parse("program Fib1; // Some comment");
        let valid_2 = parser.parse(
            "
            program Fib2; (*\n\
                some comments \n\r\
                some other comment \
            *)\
            ",
        );
        let valid_3 = parser.parse(
            "
            program Fib3; {\n\
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

        let invalid_1 = parser.parse("program Fib1; (*\n");
        let invalid_2 = parser.parse("program Fib2; {\n");

        assert!(invalid_1.is_err());
        let err = invalid_1.unwrap_err();
        assert!(format!("{:?}", err).contains("("));

        assert!(invalid_2.is_err());
        let err = invalid_2.unwrap_err();
        assert!(format!("{:?}", err).contains("{"));
    }

    #[test]
    fn string() {
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
    fn invalid_string() {
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
        let func_call = parser.parse("SomeFunc()");

        assert_eq!(
            integer.unwrap(),
            Box::new(Expr::Literal(Literal::Integer(100)))
        );
        assert_eq!(
            double.unwrap(),
            Box::new(Expr::Literal(Literal::Double(100.100)))
        );
        assert_eq!(
            string.unwrap(),
            Box::new(Expr::Literal(Literal::String(String::from(
                "'some string'"
            ))))
        );
        assert_eq!(
            boolean.unwrap(),
            Box::new(Expr::Literal(Literal::Boolean(true)))
        );
        assert_eq!(
            identifier.unwrap(),
            Box::new(Expr::Identifier(String::from("some_identifier_9")))
        );
        assert_eq!(
            func_call.unwrap(),
            Box::new(Expr::FunctionCall(FunctionCall {
                identifier: String::from("SomeFunc"),
                arguments: vec![]
            }))
        );
    }

    #[test]
    fn term_paren_recursion() {
        let parser = grammar::TermParser::new();

        let valid_1 = parser.parse("(100)");
        let valid_2 = parser.parse("((true))");

        assert_eq!(
            valid_1.unwrap(),
            Box::new(Expr::Literal(Literal::Integer(100)))
        );
        assert_eq!(
            valid_2.unwrap(),
            Box::new(Expr::Literal(Literal::Boolean(true)))
        );
    }

    #[test]
    fn invalid_term() {
        let parser = grammar::TermParser::new();

        let invalid = parser.parse("9some_identifier_9");

        assert!(invalid.is_err());
        let err = invalid.unwrap_err();
        assert!(format!("{:?}", err).contains("some_identifier_9"));
    }

    #[test]
    fn unary() {
        let parser = grammar::UnaryExprParser::new();

        let valid_1 = parser.parse("+ 5");
        let valid_2 = parser.parse("- 5");
        let valid_3 = parser.parse("not true");

        assert_eq!(
            valid_1.unwrap(),
            Box::new(Expr::Unary(
                UnaryOp::Pos,
                Box::new(Expr::Literal(Literal::Integer(5)))
            ))
        );
        assert_eq!(
            valid_2.unwrap(),
            Box::new(Expr::Unary(
                UnaryOp::Neg,
                Box::new(Expr::Literal(Literal::Integer(5)))
            ))
        );
        assert_eq!(
            valid_3.unwrap(),
            Box::new(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Literal(Literal::Boolean(true)))
            ))
        );
    }

    #[test]
    fn unary_recursion() {
        let parser = grammar::UnaryExprParser::new();

        let valid = parser.parse("- + - 5");

        assert_eq!(
            valid.unwrap(),
            Box::new(Expr::Unary(
                UnaryOp::Neg,
                Box::new(Expr::Unary(
                    UnaryOp::Pos,
                    Box::new(Expr::Unary(
                        UnaryOp::Neg,
                        Box::new(Expr::Literal(Literal::Integer(5)))
                    ))
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

    #[test]
    fn left_associativity() {
        let parser = grammar::ExprParser::new();

        let mul_assoc = parser.parse("1 * 2 / 3");
        let add_assoc = parser.parse("1 + 2 - 3");

        assert_eq!(
            mul_assoc.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(1))),
                    BinaryOp::Mul,
                    Box::new(Expr::Literal(Literal::Integer(2)))
                )),
                BinaryOp::Div,
                Box::new(Expr::Literal(Literal::Integer(3)))
            ))
        );
        assert_eq!(
            add_assoc.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(1))),
                    BinaryOp::Add,
                    Box::new(Expr::Literal(Literal::Integer(2)))
                )),
                BinaryOp::Sub,
                Box::new(Expr::Literal(Literal::Integer(3)))
            ))
        );
    }

    #[test]
    fn precedence() {
        let parser = grammar::ExprParser::new();

        let right_mul = parser.parse("1 + 2 * 3");
        let left_mul = parser.parse("1 mod 2 - 3");

        assert_eq!(
            right_mul.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Integer(1))),
                BinaryOp::Add,
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(2))),
                    BinaryOp::Mul,
                    Box::new(Expr::Literal(Literal::Integer(3)))
                )),
            ))
        );
        assert_eq!(
            left_mul.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(1))),
                    BinaryOp::Mod,
                    Box::new(Expr::Literal(Literal::Integer(2)))
                )),
                BinaryOp::Sub,
                Box::new(Expr::Literal(Literal::Integer(3))),
            ))
        );
    }

    #[test]
    fn comparison_basic() {
        let parser = grammar::ExprParser::new();

        let equal = parser.parse("5 = 5");
        let more_equal = parser.parse("10 > 5 = 5");

        assert_eq!(
            equal.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Integer(5))),
                BinaryOp::Eq,
                Box::new(Expr::Literal(Literal::Integer(5)))
            ))
        );
        assert_eq!(
            more_equal.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(10))),
                    BinaryOp::Gt,
                    Box::new(Expr::Literal(Literal::Integer(5)))
                )),
                BinaryOp::Eq,
                Box::new(Expr::Literal(Literal::Integer(5)))
            ))
        );
    }

    #[test]
    fn complex() {
        let parser = grammar::ExprParser::new();

        let complex = parser.parse("1 * 2 <> 3 - 4");

        assert_eq!(
            complex.unwrap(),
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(1))),
                    BinaryOp::Mul,
                    Box::new(Expr::Literal(Literal::Integer(2)))
                )),
                BinaryOp::Neq,
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(3))),
                    BinaryOp::Sub,
                    Box::new(Expr::Literal(Literal::Integer(4)))
                ))
            ))
        );
    }

    #[test]
    fn variable_declaration() {
        let parser = grammar::VariableDeclarationParser::new();

        let var_decl = parser.parse("some_name : integer;");
        let var_decl_init = parser.parse("some_name : boolean = true;");

        assert_eq!(
            var_decl.unwrap(),
            VariableDeclaration {
                identifier: "some_name".to_string(),
                typ: Type::Integer,
                expr: None,
            }
        );
        assert_eq!(
            var_decl_init.unwrap(),
            VariableDeclaration {
                identifier: "some_name".to_string(),
                typ: Type::Boolean,
                expr: Some(Box::new(Expr::Literal(Literal::Boolean(true)))),
            }
        );
    }

    #[test]
    fn variable_section() {
        let parser = grammar::VariableDeclarationBlockParser::new();

        let block = parser.parse(
            "var
                var_dec_1 : integer;
                var_dec_2 : double = 2.0;",
        );

        assert_eq!(
            block.unwrap(),
            vec![
                VariableDeclaration {
                    identifier: String::from("var_dec_1"),
                    typ: Type::Integer,
                    expr: None
                },
                VariableDeclaration {
                    identifier: String::from("var_dec_2"),
                    typ: Type::Double,
                    expr: Some(Box::new(Expr::Literal(Literal::Double(2.0))))
                }
            ]
        )
    }

    #[test]
    fn variable_assignment() {
        let parser = grammar::VariableAssignmentParser::new();

        let var_assign = parser.parse("some_name := 1");

        assert_eq!(
            var_assign.unwrap(),
            VariableAssignment {
                identifier: "some_name".to_string(),
                expr: Box::new(Expr::Literal(Literal::Integer(1))),
            }
        );
    }

    #[test]
    fn block_multi_statement() {
        let parser = grammar::BlockParser::new();

        let no_semicolon = parser.parse(
            "begin
                var_assign_1 := 1;
                var_assign_2 := 2
            end",
        );
        let with_semicolon = parser.parse(
            "begin
                var_assign_1 := 1;
                var_assign_2 := 2;
            end",
        );
        let single_no_semicolon = parser.parse(
            "begin
                var_assign_1 := 1
            end",
        );
        let single_with_semicolon = parser.parse(
            "begin
                var_assign_1 := 1;
            end",
        );
        let empty_block = parser.parse("begin end");
        let semicolon_block = parser.parse("begin ; ; end");

        let assignment = vec![
            Statement::VariableAssignment(VariableAssignment {
                identifier: String::from("var_assign_1"),
                expr: Box::new(Expr::Literal(Literal::Integer(1))),
            }),
            Statement::VariableAssignment(VariableAssignment {
                identifier: String::from("var_assign_2"),
                expr: Box::new(Expr::Literal(Literal::Integer(2))),
            }),
        ];
        assert_eq!(no_semicolon.unwrap(), assignment);
        assert_eq!(with_semicolon.unwrap(), assignment);

        let single_assignment = vec![Statement::VariableAssignment(VariableAssignment {
            identifier: String::from("var_assign_1"),
            expr: Box::new(Expr::Literal(Literal::Integer(1))),
        })];
        assert_eq!(single_no_semicolon.unwrap(), single_assignment);
        assert_eq!(single_with_semicolon.unwrap(), single_assignment);
        assert!(empty_block.unwrap().is_empty());
        assert!(semicolon_block.unwrap().is_empty());
    }

    #[test]
    fn matched_if_else() {
        let parser = grammar::MatchedIfParser::new();

        let if_else = parser.parse(
            "if ( 1 = 2 ) then
                some_var := 1
            else
                other_var := 2",
        );
        let invalid_if_else = parser.parse(
            "if ( 1 = 2 ) then
                some_var := 1;
            else
                other_var := 2",
        );

        assert!(matches!(
            if_else.unwrap(),
            IfElse {
                expr: _,
                if_statement: _,
                else_statement: Some(_)
            }
        ));
        assert!(invalid_if_else.is_err());
    }

    #[test]
    fn unmatched_if_else() {
        let parser = grammar::UnmatchedIfParser::new();

        let only_if = parser.parse(
            "if ( 1 = 2 ) then
                some_var := 1",
        );
        let if_else = parser.parse(
            "if ( 1 = 2 ) then
                some_var := 1
            else
                if ( 1 = 2) then
                    other_var := 2",
        );

        assert!(matches!(
            only_if.unwrap(),
            IfElse {
                expr: _,
                if_statement: _,
                else_statement: None
            }
        ));
        assert!(matches!(
            if_else.unwrap(),
            IfElse {
                expr: _,
                if_statement: _,
                else_statement: Some(e_stmt)
            } if matches!(*e_stmt, Statement::IfElse(IfElse {
                    expr: _,
                    if_statement: _,
                    else_statement: None
                }))
        ));
    }

    #[test]
    fn while_loop() {
        let matched_parser = grammar::MatchedWhileParser::new();
        let unmatched_parser = grammar::MatchedWhileParser::new();

        let input = "while ( 1 = 2 ) do
                some_var := 1";
        let matched_while_loop = matched_parser.parse(&input);
        let unmatched_while_loop = unmatched_parser.parse(&input);

        assert!(matches!(
            matched_while_loop.unwrap(),
            While {
                expr: _,
                statement: _
            }
        ));
        assert!(matches!(
            unmatched_while_loop.unwrap(),
            While {
                expr: _,
                statement: _
            }
        ));
    }

    #[test]
    fn parameter() {
        let parser = grammar::ParameterParser::new();

        let param = parser.parse("some_param : integer");
        let mult_param = parser.parse("param_1, param_2, param_3 : integer");

        assert_eq!(
            param.unwrap(),
            vec![Parameter {
                identifier: String::from("some_param"),
                typ: Type::Integer
            }]
        );
        assert_eq!(
            mult_param.unwrap(),
            vec![
                Parameter {
                    identifier: String::from("param_1"),
                    typ: Type::Integer
                },
                Parameter {
                    identifier: String::from("param_2"),
                    typ: Type::Integer
                },
                Parameter {
                    identifier: String::from("param_3"),
                    typ: Type::Integer
                }
            ]
        );
    }

    #[test]
    fn parameter_list() {
        let parser = grammar::ParameterListParser::new();

        let param_list = parser.parse("some_param : integer; param_1, param_2, param_3 : double");
        let invalid_param_list = parser.parse("some_param : integer;");

        assert_eq!(
            param_list.unwrap(),
            vec![
                Parameter {
                    identifier: String::from("some_param"),
                    typ: Type::Integer
                },
                Parameter {
                    identifier: String::from("param_1"),
                    typ: Type::Double
                },
                Parameter {
                    identifier: String::from("param_2"),
                    typ: Type::Double
                },
                Parameter {
                    identifier: String::from("param_3"),
                    typ: Type::Double
                }
            ]
        );
        assert!(invalid_param_list.is_err());
    }

    #[test]
    fn function_and_procedure() {
        let parser = grammar::FunctionDeclarationParser::new();

        let func = parser.parse(
            "function Fib(n: integer): integer;
            begin
                if (n>0) then
                    if (n<=2) then
                        Fib := 1
                    else
                        Fib := Fib(n-1) + Fib(n-2)
                else
                    Fib := 0
            end;",
        );
        let proc = parser.parse(
            "procedure SomeFunc(a, b: integer);
            var
                c : integer;
            begin
                if (a>c) then
                    b := 0
                else
                    a := 0
            end;",
        );

        assert!(matches!(
            func.unwrap(),
            FunctionDeclaration {
                identifier: _,
                parameter_list: _,
                return_type: Some(_),
                variable_declaration: _,
                body: _
            }
        ));
        assert!(matches!(
            proc.unwrap(),
            FunctionDeclaration {
                identifier: _,
                parameter_list: _,
                return_type: None,
                variable_declaration: _,
                body: _
            }
        ));
    }

    #[test]
    fn argument_list() {
        let parser = grammar::ArgumentListParser::new();

        let arg_list = parser.parse("1, 'something', 2.0, true");
        let invalid_arg_list = parser.parse("1,");

        assert_eq!(
            arg_list.unwrap(),
            vec![
                Box::new(Expr::Literal(Literal::Integer(1))),
                Box::new(Expr::Literal(Literal::String(String::from("'something'")))),
                Box::new(Expr::Literal(Literal::Double(2.0))),
                Box::new(Expr::Literal(Literal::Boolean(true))),
            ]
        );
        assert!(invalid_arg_list.is_err());
    }

    #[test]
    fn function_call() {
        let parser = grammar::FunctionCallParser::new();

        let func_call = parser.parse("SomeFunc(1, 'a string')");

        assert_eq!(
            func_call.unwrap(),
            FunctionCall {
                identifier: String::from("SomeFunc"),
                arguments: vec![
                    Box::new(Expr::Literal(Literal::Integer(1))),
                    Box::new(Expr::Literal(Literal::String(String::from("'a string'")))),
                ]
            }
        )
    }

    #[test]
    fn program() {
        let parser = grammar::ProgramParser::new();

        let fibonacci = parser.parse(
            "
            program Fibonacci;

            function Fib(n: integer): integer;
            begin
                if (n>0) then
                    if (n<=2) then
                        Fib := 1
                    else
                        Fib := Fib(n-1) + Fib(n-2)
                else
                    Fib := 0
            end;

            begin
                writeln (Fib(5));
                writeln (Fib(10));
            end.
            ",
        );

        assert!(fibonacci.is_ok());
    }
}
