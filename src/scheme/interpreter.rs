use scheme::base::*;
use scheme::base::numeric::Operator::*;
use scheme::reader::Reader;
use scheme::env::Environment;
use scheme::value::{Sexp, HostFunction1, HostFunction2};
use scheme::LispResult;
use scheme::machine::LispMachine;
use scheme::error::LispError;

use std::fs::File;
use std::process::exit;
use std::io::BufReader;
use std::io::Cursor;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            env: Environment::new(None),
        };
        interp.init_globals();
        interp
    }

    pub fn bind_synatx(&mut self, name: &str, func: HostFunction2) {
        let form = Sexp::Syntax { name: name.to_owned(), func };
        self.env.insert(name, &form);
    }

    pub fn bind_proc(&mut self, name: &str, func: HostFunction2) {
        let proc = Sexp::Procedure { name: name.to_owned(), func };
        self.env.insert(name, &proc);
    }

    pub fn bind_func(&mut self, name: &str, func: HostFunction1) {
        let proc = Sexp::Function { name: name.to_owned(), func };
        self.env.insert(name, &proc);
    }

    fn init_globals(&mut self) {
        self.bind_func("+", |args| numeric::arith_op(Add, args));
        self.bind_func("-", |args| numeric::arith_op(Sub, args));
        self.bind_func("*", |args| numeric::arith_op(Mul, args));
        self.bind_func("/", |args| numeric::arith_op(Div, args));
        self.bind_func("=", |args| numeric::compare("=", |x, y| x == y, args));
        self.bind_func("<", |args| numeric::compare("<", |x, y| x < y, args));
        self.bind_func(">", |args| numeric::compare(">", |x, y| x > y, args));
        self.bind_func("<=", |args| numeric::compare("<=", |x, y| x <= y, args));
        self.bind_func(">=", |args| numeric::compare(">=", |x, y| x >= y, args));
        self.bind_func("char=?", |args| character::compare("char=?", |x, y| x == y, args));
        self.bind_func("char<?", |args| character::compare("char<?", |x, y| x < y, args));
        self.bind_func("char>?", |args| character::compare("char>?", |x, y| x > y, args));
        self.bind_func("char<=?", |args| character::compare("char<=?", |x, y| x <= y, args));
        self.bind_func("char>=?", |args| character::compare("char>=?", |x, y| x >= y, args));
        self.bind_func("make-string", string::make_string);
        self.bind_func("string-length", string::string_length);
        self.bind_func("string-ref", string::string_ref);
        self.bind_func("string-set!", string::string_set);
        self.bind_func("car", list::car);
        self.bind_func("cdr", list::cdr);
        self.bind_func("cons", list::cons);
        self.bind_func("eq?", equality::is_eq);
        self.bind_func("eqv?", equality::is_eq);
        self.bind_func("equal?", equality::is_equal);
        self.bind_func("pair?", predicate::is_pair);
        self.bind_func("string?", predicate::is_string);
        self.bind_func("number?", predicate::is_number);
        self.bind_func("integer?", predicate::is_integer);
        self.bind_func("rational?", predicate::is_rational);
        self.bind_func("real?", predicate::is_real);
        self.bind_func("complex?", predicate::is_complex);
        self.bind_func("exact?", predicate::is_exact);
        self.bind_func("inexact?", predicate::is_inexact);
        self.bind_func("char?", predicate::is_char);
        self.bind_func("symbol?", predicate::is_symbol);
        self.bind_func("procedure?", predicate::is_procedure);
        self.bind_proc("apply", control::apply_proc);
        self.bind_proc("load", system::load);
        self.bind_proc("exit", system::exit_process);
        self.bind_synatx("define", core::define);
        self.bind_synatx("set!", core::assign);
        self.bind_synatx("quote", core::quote);
        self.bind_synatx("lambda", core::lambda);
        self.bind_synatx("if", core::if_exp);
        self.bind_synatx("define-syntax", syntax::define_syntax);

        // 加载库文件
        self.run_once("./libs/stdlib.scm");
    }

    // 运行解释器
    pub fn run(&mut self, reader: Reader) -> LispResult<Sexp> {
        let mut res = Ok(Sexp::Void);
        let mut vm = LispMachine::new(self.env.clone());
        for item in reader {
            match item {
                Ok(datum) => {
                    res = vm.eval(&datum);
                    if res.is_err() {
                        return res;
                    }
                }
                Err(_) => return item
            }
        }
        res
    }

    pub fn run_repl(&mut self) {
        let mut res_no = 0;
        let mut vm = LispMachine::new(self.env.clone());
        for item in Reader::new() {
            match item {
                Ok(datum) => {
                    match vm.eval(&datum) {
                        Ok(ref val) => match val {
                            Sexp::Void => (),
                            _ => {
                                res_no += 1;
                                let last_res = format!("${}", res_no);
                                if self.env.lookup(&last_res).is_none() {
                                    self.env.insert(&last_res, val);
                                }
                                println!("{} = {}", last_res, val);
                            }
                        },
                        Err(err) => eprintln!("Error: {}", err),
                    }
                }
                Err(LispError::EndOfInput) => break,
                Err(err) => eprintln!("Error: {}", err),
            }
        }
    }

    pub fn run_once(&mut self, path: &str) {
        let file = match File::open(path) {
            Ok(file) => file,
            Err(err) => {
                eprintln!("lagoon: {}: '{}'", err, path);
                exit(1)
            }
        };
        let mut reader = Reader::new();
        let mut input = BufReader::new(file);
        reader.set_input(&mut input);
        match self.run(reader) {
            Err(err) => eprintln!("Error: {}", err),
            _ => return,
        }
    }

    pub fn eval_string(&mut self, string: &str) -> LispResult<Sexp> {
        let mut reader = Reader::new();
        let mut input = Cursor::new(string);
        reader.set_input(&mut input);
        self.run(reader)
    }
}
