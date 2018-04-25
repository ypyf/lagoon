#![feature(nll)]

pub mod scheme;

use scheme::interpreter::Interpreter;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        Interpreter::new().run_once(args[1].as_str());
    } else {
        Interpreter::new().run_repl();
    }
}
