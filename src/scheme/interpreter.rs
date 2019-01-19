use scheme::procedure::basic;
use scheme::procedure::numeric;
use scheme::procedure::numeric::Operator::*;
use scheme::procedure::string;
use scheme::procedure::predicate;
use scheme::procedure::process;
use scheme::procedure::list;
use scheme::procedure::character;
use scheme::reader::Reader;
use scheme::types::Context;
use scheme::types::LispError;
use scheme::types::Sexp;

use std::rc::Rc;
use std::fs::File;
use std::process::exit;
use std::io::BufReader;
use std::io::Cursor;

pub struct Interpreter {
    ctx: Context,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            ctx: Context::new(),
        }
    }

    fn init_globals(&mut self) {
        self.ctx.def_proc("+", |ctx, args| numeric::arith_op(Add, ctx, args));
        self.ctx.def_proc("-", |ctx, args| numeric::arith_op(Sub, ctx, args));
        self.ctx.def_proc("*", |ctx, args| numeric::arith_op(Mul, ctx, args));
        self.ctx.def_proc("/", |ctx, args| numeric::arith_op(Div, ctx, args));
        self.ctx.def_proc("=", |ctx, args| numeric::compare("=", |x, y| x == y, ctx, args));
        self.ctx.def_proc("<", |ctx, args| numeric::compare("<", |x, y| x < y, ctx, args));
        self.ctx.def_proc(">", |ctx, args| numeric::compare(">", |x, y| x > y, ctx, args));
        self.ctx.def_proc("<=", |ctx, args| numeric::compare("<=", |x, y| x <= y, ctx, args));
        self.ctx.def_proc(">=", |ctx, args| numeric::compare(">=", |x, y| x >= y, ctx, args));
        self.ctx.def_proc("char=?", |ctx, args| character::compare("char=?", |x, y| x == y, ctx, args));
        self.ctx.def_proc("char<?", |ctx, args| character::compare("char<?", |x, y| x < y, ctx, args));
        self.ctx.def_proc("char>?", |ctx, args| character::compare("char>?", |x, y| x > y, ctx, args));
        self.ctx.def_proc("char<=?", |ctx, args| character::compare("char<=?", |x, y| x <= y, ctx, args));
        self.ctx.def_proc("char>=?", |ctx, args| character::compare("char>=?", |x, y| x >= y, ctx, args));
        self.ctx.def_proc("make-string", string::make_string);
        self.ctx.def_proc("string-length", string::string_length);
        self.ctx.def_proc("string-ref", string::string_ref);
        self.ctx.def_proc("string-set!", string::string_set);
        self.ctx.def_proc("car", list::car);
        self.ctx.def_proc("cdr", list::cdr);
        self.ctx.def_proc("cons", list::cons);
        self.ctx.def_proc("pair?", predicate::is_pair);
        self.ctx.def_proc("string?", predicate::is_string);
        self.ctx.def_proc("number?", predicate::is_number);
        self.ctx.def_proc("integer?", predicate::is_integer);
        self.ctx.def_proc("rational?", predicate::is_rational);
        self.ctx.def_proc("real?", predicate::is_real);
        self.ctx.def_proc("complex?", predicate::is_complex);
        self.ctx.def_proc("exact?", predicate::is_exact);
        self.ctx.def_proc("inexact?", predicate::is_inexact);
        self.ctx.def_proc("char?", predicate::is_char);
        self.ctx.def_proc("symbol?", predicate::is_symbol);
        self.ctx.def_proc("procedure?", predicate::is_procedure);
        self.ctx.def_proc("quit", process::quit);
        self.ctx.def_synatx("define", basic::define);
        self.ctx.def_synatx("set!", basic::assign);
        self.ctx.def_synatx("quote", basic::quote);
        self.ctx.def_synatx("lambda", basic::lambda);
    }

    // 运行解释器
    // 只打印错误消息，不回显正确结果
    pub fn run(&mut self, reader: Reader) {
        self.ctx.enter_scope();
        self.init_globals();
        for item in reader {
            match item {
                Ok(expr) => if let Err(err) = self.ctx.eval(&expr) {
                    eprintln!("{}", err)
                }
                Err(err) => eprintln!("{}", err)
            }
        }
    }

    pub fn run_repl(&mut self) {
        let mut res_no = 0;
        self.ctx.enter_scope();
        self.init_globals();
        for expr in Reader::new() {
            match expr {
                Ok(ref sexp) => {
                    self.ctx.set_current_expr(Rc::new(sexp.clone()));
                    match self.ctx.eval(sexp) {
                        Ok(val) => match val {
                            Sexp::Void => (),
                            _ => {
                                res_no += 1;
                                let last_res = format!("${}", res_no);
                                if self.ctx.lookup(&last_res).is_none() {
                                    self.ctx.define_variable(&last_res, &val);
                                }
                                println!("{} = {}", last_res, val);
                            }
                        },
                        Err(err) => eprintln!("{}", err),
                    }
                }
                Err(LispError::EndOfInput) => break,
                Err(err) => eprintln!("{}", err),
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
        self.run(reader);
    }

    pub fn eval_string(&mut self, string: &str) {
        let mut reader = Reader::new();
        let mut input = Cursor::new(string);
        reader.set_input(&mut input);
        self.run(reader);
    }
}
