use scheme::base::syntax;
use scheme::base::numeric;
use scheme::base::numeric::Operator::*;
use scheme::base::string;
use scheme::base::predicate;
use scheme::base::system;
use scheme::base::list;
use scheme::base::character;
use scheme::base::control;
use scheme::base::equality;
use scheme::reader::Reader;
use scheme::types::Context;
use scheme::types::LispResult;
use scheme::types::LispError;
use scheme::types::Sexp;

use std::fs::File;
use std::process::exit;
use std::io::BufReader;
use std::io::Cursor;

pub struct Interpreter {
    ctx: Context,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            ctx: Context::new(None),
        };
        interp.init_globals();
        interp
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
        self.ctx.def_proc("eq?", equality::is_eq);
        self.ctx.def_proc("eqv?", equality::is_eq);
        self.ctx.def_proc("equal?", equality::is_equal);
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
        self.ctx.def_proc("apply", control::apply);
        self.ctx.def_proc("exit", system::exit_process);
        self.ctx.def_synatx("define", syntax::define);
        self.ctx.def_synatx("set!", syntax::assign);
        self.ctx.def_synatx("quote", syntax::quote);
        self.ctx.def_synatx("lambda", syntax::lambda);
        self.ctx.def_synatx("if", syntax::if_exp);
        self.ctx.def_synatx("define-syntax", syntax::define_syntax);

        // 加载库文件
        // TODO 配置运行时根目录
        self.run_once("./libs/stdlib.scm");
    }

    // 运行解释器
    pub fn run(&mut self, reader: Reader) -> LispResult<Sexp> {
        let mut res = Ok(Sexp::Void);
        for item in reader {
            match item {
                Ok(datum) => {
                    res = self.ctx.eval(&datum);
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
        for item in Reader::new() {
            match item {
                Ok(datum) => {
                    match self.ctx.eval(&datum) {
                        Ok(ref val) => match val {
                            Sexp::Void => (),
                            _ => {
                                res_no += 1;
                                let last_res = format!("${}", res_no);
                                if self.ctx.lookup(&last_res).is_none() {
                                    self.ctx.insert(&last_res, val);
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
        match self.run(reader) {
            Err(err) => eprintln!("{}", err),
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
