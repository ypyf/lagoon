use scheme::primitives;
use scheme::reader::Reader;
use scheme::types::Context;
use scheme::types::LispError;
use scheme::types::Sexp;

use std::fs::File;
use std::io::BufReader;

pub struct Interpreter {
    context: Context,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: Context::new(),
        }
    }

    fn set_globals(&mut self) {
        self.context.define_proc("+", primitives::plus);
        self.context.define_proc("-", primitives::subtract);
        self.context.define_proc("*", primitives::multiply);
        self.context.define_proc("quit", primitives::quit);
        self.context.define_proc("exit", primitives::quit);
        self.context.define_synatx("define", primitives::define);
        self.context.define_synatx("set!", primitives::assign);
        self.context.define_synatx("quote", primitives::quote);
    }

    pub fn run_repl(&mut self) {
        let mut lisp_reader = Reader::new();
        self.context.enter_scope();
        self.set_globals();
        loop {
            match lisp_reader.read() {
                Ok(sexp) => {
                    let result = self.context.eval(sexp);
                    match result {
                        Ok(val) => match *val {
                            Sexp::Void => (),
                            _ => println!("{}", val),
                        },
                        Err(err) => println!("{}", err),
                    }
                }
                Err(LispError::EndOfInput) => break,
                Err(err) => println!("{}", err),
            }
        }
    }

    pub fn run_once(&mut self, path: &str) {
        let file = File::open(path).expect("no such file or directory");
        let mut _reader = BufReader::new(file);
        let mut lisp_reader = Reader::new();
        self.context.enter_scope();
        self.set_globals();
        loop {
            match lisp_reader.read() {
                Ok(sexp) => {
                    let result = self.context.eval(sexp);
                    match result {
                        Ok(val) => match *val {
                            Sexp::Void => (),
                            _ => println!("{}", val),
                        },
                        Err(err) => println!("{}", err),
                    }
                }
                Err(LispError::EndOfInput) => break,
                Err(err) => println!("{}", err),
            }
        }
    }
}
