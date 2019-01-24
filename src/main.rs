#![feature(nll)]

pub mod scheme;

use scheme::interpreter::Interpreter;

fn main() {
    let mut interp = Interpreter::new();
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        interp.run_once(args[1].as_str());
    } else {
        interp.run_repl();
    }
}
