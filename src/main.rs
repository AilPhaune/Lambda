pub mod data;
pub mod eval;
pub mod expr;
pub mod parse;
pub mod repl;
pub mod type_check;

fn main() {
    repl::repl();
}
