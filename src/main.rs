#![feature(nll)]

pub mod scheme;

use scheme::interpreter::Interpreter;

fn main() {
    let mut iterp = Interpreter::new();
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        iterp.run_once(args[1].as_str());
    } else {
        iterp.run_repl();
    }
}
