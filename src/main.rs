use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

pub mod ast;

fn main() {
    // println!("{}", grammar::TermParser::new().parse("22").unwrap());
    let out = format!("{:?}", grammar::ExprParser::new().parse("(10 + 22) * (12 / 70)").unwrap());
    println!("{out}");
}
