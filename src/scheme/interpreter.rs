use scheme::primitives::basic;
use scheme::primitives::arith;
use scheme::primitives::list;
use scheme::reader::Reader;
use scheme::types::Context;
use scheme::types::LispError;
use scheme::types::Sexp;

use std::fs::File;
use std::io::BufReader;
use std::rc::Rc;

pub struct Interpreter {
    context: Context,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: Context::new(),
        }
    }

    fn init_globals(&mut self) {
        self.context.define_proc("+", arith::plus);
        self.context.define_proc("-", arith::subtract);
        self.context.define_proc("*", arith::multiply);
        self.context.define_proc("/", arith::division);
        self.context.define_proc("car", list::car);
        self.context.define_proc("cdr", list::cdr);
        self.context.define_proc("cons", list::cons);
        self.context.define_proc("quit", basic::quit);
        self.context.define_synatx("define", basic::define);
        self.context.define_synatx("set!", basic::assign);
        self.context.define_synatx("quote", basic::quote);
        self.context.define_synatx("lambda", basic::lambda);
    }

    pub fn run_repl(&mut self) {
        let mut res_no = 0;
        self.context.enter_scope();
        self.init_globals();
        loop {
            match Reader::new().read() {
                Ok(ref sexp) => {
                    self.context.set_current_expr(Rc::new(sexp.clone()));
                    match self.context.eval(sexp) {
                        Ok(val) => match val {
                            Sexp::Void => (),
                            _ => {
                                res_no += 1;
                                let last_res = format!("${}", res_no);
                                if self.context.lookup(&last_res).is_none() {
                                    self.context.define_variable(&last_res, &val);
                                }
                                println!("{} = {}", last_res, val);
                            }
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
        self.init_globals();
        loop {
            match lisp_reader.read() {
                Ok(sexp) => {
                    let result = self.context.eval(&sexp);
                    match result {
                        Ok(val) => match val {
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
