use scheme::types::Context;
use scheme::types::Sexp;
use scheme::types::Sexp::Number;
use scheme::types::LispResult;

use std::process::exit;

pub fn exit_process(_context: &mut Context, args: &[Sexp]) -> LispResult<Sexp> {
    for arg in args {
        match arg {
            Number(n) => exit(n.clone() as i32),
            _ => exit(0),
        }
    }
    exit(0)
}