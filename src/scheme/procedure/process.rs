use scheme::types::Context;
use scheme::types::Sexp;
use scheme::types::Sexp::Number;
use scheme::types::LispResult;

use std::process::exit;

pub fn quit(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    for arg in args {
        match arg {
            Number(n) => exit(n as i32),
            _ => exit(0),
        }
    }
    exit(0)
}
