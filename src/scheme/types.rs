use std::collections::HashMap;
use std::collections::LinkedList;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::str;

pub type LispResult = Result<Sexp, LispError>;

type Env = LinkedList<HashMap<String, Sexp>>;

#[derive(Debug, Clone)]
pub struct Context {
    env: Env,
    current: Rc<Sexp>,
}

#[allow(dead_code)]
impl Context {
    pub fn new() -> Self {
        Context { env: Env::new(), current: Rc::new(Sexp::Void) }
    }

    pub fn get_env(&self) -> &Env {
        &self.env
    }

    pub fn get_current_expr(&self) -> Rc<Sexp> {
        self.current.clone()
    }

    pub fn set_current_expr(&mut self, expr: Rc<Sexp>) {
        self.current = expr
    }

    pub fn enter_scope(&mut self) {
        self.env.push_back(HashMap::new())
    }

    pub fn leave_scope(&mut self) {
        self.env.pop_back();
    }

    pub fn define_variable(&mut self, name: &str, sexp: Sexp) {
        let current = self.env.back_mut().unwrap();
        current.insert(name.to_owned(), sexp);
    }

    pub fn define_synatx(&mut self, name: &str, func: Function) {
        let current = self.env.back_mut().unwrap();
        current.insert(
            name.to_owned(),
            Sexp::Function {
                name: name.to_owned(),
                special: true,
                func,
            },
        );
    }

    pub fn define_proc(&mut self, name: &str, func: Function) {
        let current = self.env.back_mut().unwrap();
        current.insert(
            name.to_owned(),
            Sexp::Function {
                name: name.to_owned(),
                special: false,
                func,
            },
        );
    }

    pub fn lookup(&self, name: &str) -> Option<Sexp> {
        for current in &self.env {
            match current.get(name) {
                Some(val) => return Some(val.clone()),
                None => continue,
            }
        }
        None
    }

    pub fn eval(&mut self, expr: &Sexp) -> LispResult {
        use self::Sexp::*;
        use self::LispError::*;

        match expr {
            Symbol(sym) => match self.lookup(sym.as_str()) {
                Some(val) => {
                    Ok(val)
                }
                None => Err(Undefined(sym.to_string())),
            },
            List(v, t) => {
                self.current = Rc::new(expr.clone());
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

        let mut args = exprs;
        match proc {
            Function { name: _, special, func } => {
                if special {
                    if *args.last().unwrap() == Sexp::Nil {
                        args.pop();
                    }
                    func(self, args)
                } else {
                    if *args.last().unwrap() != Sexp::Nil {
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
                if *args.last().unwrap() != Sexp::Nil {
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
                context.enter_scope();
                match vararg {
                    Some(sym) => {
                        for (k, v) in params.iter().zip(args.iter()) {
                            context.define_variable(k, (*v).clone());
                        }
                        let val = if nargs == nparams {
                            Nil
                        } else {
                            List(args[nparams..].to_vec(), Rc::new(Nil))
                        };
                        context.define_variable(&sym, val);
                    }
                    _ => {
                        for (k, v) in params.iter().zip(args.iter()) {
                            context.define_variable(k, (*v).clone());
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
    Str(String),
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
            (Str(s1), Str(s2)) => s1 == s2,
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
            Str(n) => write!(f, "{:?}", n), // 字符串输出时显示双引号
            Function { name, .. } => write!(f, "#<procedure:{}>", name),
            Closure { name, .. } => {
                if name.len() > 0 {
                    write!(f, "#<procedure:{}>", name)
                } else {
                    write!(f, "#<procedure>")
                }
            }
            List(first, second) => {
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
    BadSyntax(String, Option<String>, Context),
    Undefined(String),
    ApplyError(String),
    ArityMismatch(String, usize, usize),
    TypeMismatch(String, String),
    NotImplemented(String),
}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LispError::*;

        match self {
            EndOfInput => write!(f, ""),
            Interrupted => write!(f, "User interrupt"),
            ReadError(err) => write!(f, "read: {}", err),
            BadSyntax(sym, err, ctx) => {
                let msg = if let Some(e) = err {
                    format!("bad syntax ({})", e)
                } else {
                    "bad syntax".to_owned()
                };
                write!(f, "{}: {}\n in: {}", sym, msg, ctx.get_current_expr())
            },
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
            NotImplemented(sym) => write!(f, "{}: not implemented", sym),
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
            BadSyntax(_, _, _) => "bad syntax",
            Undefined(_) => "undefined identifier",
            ApplyError(_) => "application error",
            ArityMismatch(_, _, _) => "arity mismatch",
            TypeMismatch(_, _) => "type mismatch",
            NotImplemented(_) => "not implemented",
        }
    }
}

// TODO 补充完整ASCII中所有的不可打印字符
// FIXME newline应该根据平台决定是linefeed还是return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
#[allow(dead_code)]
pub fn name_to_char(name: &str) -> Option<char> {
    if name.len() > 1 {
        // 字符名不区分大小写
        match name.to_lowercase().as_str() {
            "backspace" => Some('\x08'),
            "space" => Some(' '),
            "newline" => Some('\n'),
            "return" => Some('\r'),
            _ => None,
        }
    } else {
        name.chars().next()
    }
}

// TODO 参看 name_to_char
pub fn char_to_name(ch: char) -> String {
    match ch {
        '\x08' => "backspace".to_owned(),
        '\n' => "newline".to_owned(),
        '\r' => "return".to_owned(),
        ' ' => "space".to_owned(),
        _ => ch.to_string(),
    }
}
