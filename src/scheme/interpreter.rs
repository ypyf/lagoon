use scheme::primitives;
use scheme::reader::Reader;
use scheme::types::Context;
use scheme::types::LispError;
use scheme::types::Sexp;
use std::fs::File;
use std::io::{self, BufReader};

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
        self.context.define_procedure("+", primitives::plus);
        self.context.define_procedure("-", primitives::subtract);
        self.context.define_procedure("*", primitives::multiply);
        self.context.define_synatx("define", primitives::define);
        self.context.define_synatx("quote", primitives::quote);
    }

    pub fn run_repl(&mut self) {
        let stdin = io::stdin();
        let mut stdin_lock = stdin.lock();
        let mut reader = Reader::new(&mut stdin_lock, true);
        self.context.enter_scope();
        self.set_globals();
        loop {
            match reader.read() {
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
                Err(err) => println!("{}", err),
            }
        }
    }

    pub fn run_once<'a>(&mut self, path: &'a str) {
        match File::open(path) {
            Ok(f) => {
                let mut file = BufReader::new(f);
                let mut reader = Reader::new(&mut file, false);
                self.context.enter_scope();
                self.set_globals();
                loop {
                    match reader.read() {
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
            Err(err) => println!("{}", err),
        }
    }
}
