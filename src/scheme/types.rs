use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::str;
use std::cell::RefCell;

pub type LispResult = Result<Sexp, LispError>;

type Env = Vec<HashMap<String, Rc<RefCell<Sexp>>>>;

#[derive(Debug, Clone)]
pub struct Context {
    env: Rc<RefCell<Env>>,
    last_expr: Rc<Sexp>,
    current_proc: Rc<Sexp>, // 当前apply的过程
}

#[allow(dead_code)]
impl Context {
    pub fn new() -> Self {
        Context {
            env: Rc::new(RefCell::new(Env::new())),
            last_expr: Rc::new(Sexp::Void),
            current_proc: Rc::new(Sexp::Void),
        }
    }

    pub fn get_current_expr(&self) -> Rc<Sexp> {
        self.last_expr.clone()
    }

    pub fn set_current_expr(&mut self, expr: Rc<Sexp>) {
        self.last_expr = expr
    }

    pub fn get_current_proc(&self) -> Rc<Sexp> {
        self.current_proc.clone()
    }

    pub fn set_current_proc(&mut self, expr: Rc<Sexp>) {
        self.current_proc = expr
    }

    pub fn enter_scope(&mut self) {
        self.env.borrow_mut().push(HashMap::new())
    }

    pub fn leave_scope(&mut self) {
        self.env.borrow_mut().pop();
    }

    pub fn define_variable(&mut self, name: &str, val: &Sexp) {
        let var = Rc::new(RefCell::new(val.clone()));
        self.env.borrow_mut().last_mut().unwrap().insert(name.to_owned(), var);
    }

    pub fn set_variable(&mut self, name: &str, val: &Sexp) -> bool {
        if let Some(var) = self.lookup(name) {
            *var.borrow_mut() = val.clone();
            true
        } else {
            false
        }
    }

    pub fn def_synatx(&mut self, name: &str, func: Function) {
        let form = Sexp::Function {
            name: name.to_owned(),
            special: true,
            func,
        };
        self.define_variable(name, &form);
    }

    pub fn def_proc(&mut self, name: &str, func: Function) {
        let proc = Sexp::Function {
            name: name.to_owned(),
            special: false,
            func,
        };
        self.define_variable(name, &proc);
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<RefCell<Sexp>>> {
        for current in self.env.borrow_mut().iter().rev() {
            if let Some(val) = current.get(name) {
                return Some(val.clone());
            }
        }
        None
    }

    pub fn eval(&mut self, expr: &Sexp) -> LispResult {
        use self::Sexp::*;
        use self::LispError::*;

        match expr {
            Symbol(name) => match self.lookup(name.as_str()) {
                Some(val) => {
                    Ok(val.borrow().clone())
                }
                None => Err(Undefined(name.to_string())),
            },
            Nil => return Err(ApplyError("missing procedure expression".to_owned())),
            List(v, t) => {
                self.last_expr = Rc::new(expr.clone());
                if v.is_empty() {
                    return Err(ApplyError("missing procedure expression".to_owned()));
                }
                let proc = self.eval(&v[0])?;
                let mut args = v[1..].to_vec();
                args.push((**t).clone());
                self.apply(proc, args)
            }
            _ => Ok(expr.clone()), // 其它表达式求值到其本身
        }
    }

    fn apply(&mut self, proc: Sexp, exprs: Vec<Sexp>) -> LispResult {
        use self::Sexp::*;
        use self::LispError::*;

        self.set_current_proc(Rc::new(proc.clone()));
        let mut args = exprs;
        match proc {
            Function { name: _, special, func } => {
                if special {
                    if *args.last().unwrap() == Nil {
                        args.pop();
                    }
                    func(self, args)
                } else {
                    if *args.last().unwrap() != Nil {
                        return Err(BadSyntax("apply".to_owned(), None, self.clone()));
                    }
                    args.pop();
                    let mut vals = Vec::with_capacity(args.len());
                    for arg in args {
                        let val = self.eval(&arg)?;
                        vals.push(val);
                    }
                    func(self, vals)
                }
            }
            Closure { name, params, vararg, body, mut context } => {
                if *args.last().unwrap() != Nil {
                    return Err(BadSyntax("apply".to_owned(), None, self.clone()));
                }
                args.pop();

                let func_name = if name.is_empty() {
                    "#<procedure>".to_string()
                } else {
                    name
                };

                let nparams = params.len();
                let nargs = args.len();
                if nargs < nparams && vararg.is_some() {
                    // TODO message: expected least nparams...
                    return Err(ArityMismatch(func_name, nparams, nargs));
                } else if nargs != nparams && vararg.is_none() {
                    return Err(ArityMismatch(func_name, nparams, nargs));
                }

                let mut vals = Vec::with_capacity(args.len());
                for arg in &args {
                    let val = self.eval(arg)?;
                    vals.push(val);
                }

                context.enter_scope();
                match vararg {
                    Some(sym) => {
                        for (k, v) in params.iter().zip(vals.iter()) {
                            context.define_variable(k, v);
                        }
                        let ref val = if nargs == nparams {
                            Nil
                        } else {
                            List(vals[nparams..].to_vec(), Rc::new(Nil))
                        };
                        context.define_variable(&sym, val);
                    }
                    _ => {
                        for (k, v) in params.iter().zip(vals.iter()) {
                            context.define_variable(k, v);
                        }
                    }
                }
                // TODO tail call optimization
                let (last, elements) = body.split_last().unwrap();
                for expr in elements {
                    context.eval(&expr).unwrap();
                }
                let res = context.eval(last);
                context.leave_scope();
                res
            }
            _ => Err(ApplyError(format!("not a procedure: {}", proc))),
        }
    }
}

pub type Function = fn(&mut Context, Vec<Sexp>) -> LispResult;

#[derive(Debug, Clone)]
pub enum Sexp {
    Void,
    Nil,
    Char(char),
    Str(Rc<RefCell<String>>, bool),
    True,
    False,
    Number(i64),
    Symbol(String),
    List(Vec<Sexp>, Rc<Sexp>),
    Function {
        name: String,
        special: bool,
        func: Function,
    },
    // Vector(Vec<&'a Sexp<'a>>),
    Closure {
        name: String,
        params: Vec<String>,
        vararg: Option<String>,
        body: Vec<Sexp>,
        context: Context,
    },
}

// See also r5rs 6.1 (eqv? obj1 obj2)
// FIXME 需要完善函数、列表和字符串
impl<'a> PartialEq for Sexp {
    fn eq(&self, other: &Sexp) -> bool {
        use self::Sexp::*;

        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Symbol(s1), Symbol(s2)) => s1 == s2, // (string=? (symbol->string obj1) (symbol->string obj2))
            (Number(n1), Number(n2)) => n1 == n2, // (= obj1 obj2)
            (Char(c1), Char(c2)) => c1 == c2,     // (char=? obj1 obj2)
            (Str(s1, _), Str(s2, _)) => s1 == s2,
            (List(v1, b1), List(v2, b2)) => v1 == v2 && b1 == b2,
            (
                Function {
                    name: n1,
                    special: _,
                    func: _,
                },
                Function {
                    name: n2,
                    special: _,
                    func: _,
                },
            ) => n1 == n2, // FIXME
            _ => false,
        }
    }
}

// 实现 "{}"
impl<'a> fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Sexp::*;

        match self {
            Void => write!(f, "#<void>"),
            Nil => write!(f, "()"),
            True => write!(f, "#t"),
            False => write!(f, "#f"),
            Number(n) => write!(f, "{}", n),
            Symbol(n) => write!(f, "{}", n),
            Char(n) => write!(f, "#\\{}", char_to_name(*n)),
            Str(n, _) => write!(f, "{:?}", n.borrow()), // 字符串输出时显示双引号
            Function { name, .. } => write!(f, "#<procedure:{}>", name),
            Closure { name, .. } => {
                if name.is_empty() {
                    write!(f, "#<procedure>")
                } else {
                    write!(f, "#<procedure:{}>", name)
                }
            }
            List(first, second) => {
                if first.is_empty() {
                    return write!(f, "{}", second);
                }
                let mut datum = String::with_capacity(first.len() + 2);
                datum.push('(');
                for expr in first.iter() {
                    datum.push_str(format!("{} ", expr).as_str());
                }

                let mut exprs = (*second).clone();
                loop {
                    match (*exprs).clone() {
                        List(init, last) => {
                            for expr in init.iter() {
                                datum.push_str(format!("{} ", expr).as_str());
                            }
                            exprs = last;
                        }
                        Nil => { break; }
                        _ => {
                            datum.push_str(format!(". {} ", exprs).as_str());
                            break;
                        }
                    }
                }
                // 删除多余空格
                if datum.len() > 1 {
                    datum.pop();
                }
                datum.push(')');
                write!(f, "{}", datum)
            }
        }
    }
}

#[derive(Debug)]
pub enum LispError {
    EndOfInput,
    Interrupted,
    ReadError(String),
    AssignError(String, Context),
    BadSyntax(String, Option<String>, Context),
    IndexOutOfRange(String, usize, usize, usize),
    Undefined(String),
    ApplyError(String),
    ArityMismatch(String, usize, usize),
    TypeMismatch(String, String),
}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LispError::*;

        match self {
            EndOfInput => write!(f, ""),
            Interrupted => write!(f, "User interrupt"),
            ReadError(err) => write!(f, "read: {}", err),
            AssignError(err, _ctx) => write!(f, "set!: {}", err),
            BadSyntax(sym, err, ctx) => {
                let msg = if let Some(e) = err {
                    format!("bad syntax ({})", e)
                } else {
                    "bad syntax".to_owned()
                };
                write!(f, "{}: {}\n in: {}", sym, msg, ctx.get_current_expr())
            }
            IndexOutOfRange(sym, index, lower, upper) =>
                write!(f, "{}: index is out of range\n index: {}\n valid range: [{}, {}]", sym, index, lower, upper),
            Undefined(sym) => write!(
                f,
                "{}: undefined;\n cannot reference undefined identifier",
                sym
            ),
            ApplyError(err) => write!(f, "application: {}", err),
            ArityMismatch(sym, expected, given) => write!(
                f,
                "{}: arity mismatch;\n the expected number of arguments does not match the given number\n expected: at least {}\n given: {}",
                sym, expected, given
            ),
            TypeMismatch(expected, given) => write!(f, "type mismatch: expected: {} given: {}", expected, given),
        }
    }
}

impl Error for LispError {
    fn description(&self) -> &str {
        use self::LispError::*;
        match self {
            EndOfInput => "end of input",
            Interrupted => "user interrupt",
            ReadError(_) => "read error",
            AssignError(_, _) => "set! error",
            BadSyntax(_, _, _) => "bad syntax",
            Undefined(_) => "undefined identifier",
            IndexOutOfRange(_, _, _, _) => "index out of range",
            ApplyError(_) => "application error",
            ArityMismatch(_, _, _) => "arity mismatch",
            TypeMismatch(_, _) => "type mismatch",
        }
    }
}

// 把字符名称转换成ASCII
// TODO 补充完整ASCII中所有的不可打印字符
// #\newline应该根据平台决定是#\linefeed还是#\return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
pub fn name_to_char(name: &str) -> Option<char> {
    match name.to_lowercase().as_str() {
        "altmode" => Some('\x1b'), // ESC
        "backnext" => Some('\x1f'), // US
        "backspace" => Some('\x08'), // BS
        "call" => Some('\x1a'), // SUB
        "linefeed" => Some('\x0a'), // LF
        "page" => Some('\x0c'), // FF
        "return" => Some('\x0d'), // CR
        "rubout" => Some('\x7f'), // DEL
        "space" => Some('\x20'),
        "tab" => Some('\x09'), // HT
        "newline" => Some('\n'),
        _ => if name.chars().count() == 1 {
            // 单个unicode字符
            name.chars().next()
        } else {
            None
        }
    }
}

// 把字符转换成对应的名称
pub fn char_to_name(ch: char) -> String {
    match ch {
        '\x1b' => "altmode".to_string(),
        '\x1f' => "backnext".to_string(),
        '\x08' => "backspace".to_string(),
        '\x1a' => "call".to_string(),
        '\x0c' => "page".to_string(),
        '\x0d' => "return".to_string(),
        '\x7f' => "rubout".to_string(),
        '\x20' => "space".to_string(),
        '\x09' => "tab".to_string(),
        '\n' => "newline".to_string(),
        _ => ch.to_string(),
    }
}
