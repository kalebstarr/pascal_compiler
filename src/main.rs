use lalrpop_util::lalrpop_mod;

fn main() {
    lalrpop_mod!(
        #[allow(clippy::ptr_arg)]
        #[rustfmt::skip]
        grammar
    );
}
