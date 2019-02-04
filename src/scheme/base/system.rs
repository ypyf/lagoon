use scheme::context::Context;
use scheme::reader::Reader;
use scheme::data::LispResult;
use scheme::data::value::Sexp;
use scheme::data::value::Sexp::*;
use scheme::data::error::LispError::*;

use std::process::exit;
use std::fs::File;
use std::io::BufReader;

pub fn exit_process(_context: &mut Context, args: &[Sexp]) -> LispResult<Sexp> {
    for arg in args {
        match arg {
            Number(n) => exit(n.clone() as i32),
            _ => exit(0),
        }
    }
    exit(0)
}

pub fn load(ctx: &mut Context, args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    // TODO 可以接受environment-specifier作为第二个参数
    if args.len() != 1 {
        return Err(ArityMismatch("load".to_owned(), 1, arity));
    }

    let filename = &args[0];
    if let Str(path, _) = filename {
        let file = match File::open(path.borrow().as_str()) {
            Ok(file) => file,
            Err(err) => return Err(SystemError(err.to_string())),
        };
        let mut res = Ok(Void);
        let mut reader = Reader::new();
        let mut input = BufReader::new(file);
        reader.set_input(&mut input);
        for item in reader {
            match item {
                Ok(expr) => {
                    res = ctx.eval(&expr);
                    if res.is_err() {
                        return res;
                    }
                }
                Err(_) => return item
            }
        }
        res
    } else {
        Err(TypeMismatch("string".to_owned(), format!("{}", filename)))
    }
}
