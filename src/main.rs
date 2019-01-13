#![feature(nll)]

pub mod scheme;

use scheme::interpreter::Interpreter;

fn main() {
    Interpreter::new().run_repl();
}
