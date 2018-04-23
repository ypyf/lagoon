use std::collections::HashMap;
use std::collections::LinkedList;
use std::error::Error;
use std::fmt;
use std::str;

pub type LispResult = Result<Sexp, LispError>;

type Env = LinkedList<HashMap<String, Sexp>>;

pub struct Context {
    env: Env,
}

impl Context {
    pub fn new() -> Self {
        Context { env: Env::new() }
    }

    pub fn enter_scope(&mut self) {
        self.env.push_back(HashMap::new())
    }

    pub fn leave_scope(&mut self) {
        self.env.pop_back();
    }

    pub fn define_variable<'k>(&mut self, name: &'k str, sexp: Sexp) {
        let current = self.env.back_mut().unwrap();
        current.insert(name.to_owned(), sexp);
    }

    pub fn define_synatx<'k>(&mut self, name: &'k str, func: Function) {
        let current = self.env.back_mut().unwrap();
        current.insert(
            name.to_owned(),
            Sexp::Function {
                name: name.to_owned(),
                special: true,
                func: func,
            },
        );
    }

    pub fn define_procedure<'k>(&mut self, name: &'k str, func: Function) {
        let current = self.env.back_mut().unwrap();
        current.insert(
            name.to_owned(),
            Sexp::Function {
                name: name.to_owned(),
                special: false,
                func: func,
            },
        );
    }

    pub fn lookup<'k>(&self, name: &'k str) -> Option<&Sexp> {
        for current in &self.env {
            match current.get(name) {
                Some(val) => return Some(val),
                None => continue,
            }
        }
        None
    }

    pub fn eval<'s>(&mut self, expr: &'s Sexp) -> LispResult {
        use self::LispError::*;
        use self::Sexp::*;

        match expr {
            Symbol(sym) => match self.lookup(sym.as_str()) {
                Some(val) => Ok(val.clone()),
                None => Err(Undefined(sym.clone())),
            },
            List(v, t) => {
                if v.is_empty() {
                    return Ok(Nil); // follows MIT Scheme
                }
                let first = self.eval(&v[0])?;
                match first {
                    Function {
                        name: _,
                        special,
                        func,
                    } => self.apply(func, special, &v[1..], t),
                    _ => Err(Application),
                }
            }
            _ => Ok(expr.clone()), // 其它表达式求值到其本身
        }
    }

    fn apply(&mut self, func: Function, special: bool, exprs: &[Sexp], last: &Sexp) -> LispResult {
        if special {
            func(self, exprs)
        } else {
            if *last != Sexp::Nil {
                return Err(LispError::BadSyntax("apply".to_owned(), "".to_owned()));
            }
            let mut args = Vec::with_capacity(exprs.len());
            for expr in exprs {
                args.push(self.eval(expr)?)
            }
            func(self, &args)
        }
    }
}

pub type Function = fn(&mut Context, &[Sexp]) -> LispResult;

#[derive(Clone)]
pub enum Sexp {
    Void,
    Nil, // ()
    Char(char),
    Str(String),
    True,
    False,
    Number(i64),
    Symbol(String),
    List(Box<[Sexp]>, Box<Sexp>),
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
        env: Env,
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
            Closure { name, .. } => write!(f, "#<procedure:{}>", name),
            List(exprs, tail) => {
                let mut datum = String::with_capacity(exprs.len() + 2);
                datum.push('(');
                for expr in exprs.iter() {
                    datum.push_str(format!("{} ", expr).as_str());
                }
                if **tail != Sexp::Nil {
                    // 添加尾部的非空表
                    datum.push_str(format!(". {}", **tail).as_str());
                } else if datum.len() > 1 {
                    // 删除最后的空格
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
    ParseError(String),
    BadSyntax(String, String),
    Undefined(String),
    Application,
    ArityMismatch(String, usize, usize),
    TypeMismatch(String, String),
}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LispError::*;

        match self {
            ParseError(err) => write!(f, "read: {}", err),
            BadSyntax(sym, err) => write!(f, "{}: bad syntax {}", sym, err),
            Undefined(sym) => write!(
                f,
                "{}: undefined;\n cannot reference undefined identifier",
                sym
            ),
            Application => write!(
                f,
                "application: not a procedure;\n expected a procedure that can be applied to arguments",
            ),
            ArityMismatch(sym, expected, given) => write!(
                f,
                "{}: arity mismatch;\n the expected number of arguments does not match the given number\n expected: at least {}\n given: {}",
                sym, expected, given
            ),
            TypeMismatch(expected, given) => {
                write!(f, "type mismatch: expected: {} given: {}", expected, given)
            }
        }
    }
}

impl Error for LispError {
    fn description(&self) -> &str {
        match self {
            LispError::ParseError(_) => "read error",
            LispError::BadSyntax(_, _) => "bad syntax",
            LispError::Undefined(_) => "undefined identifier",
            LispError::Application => "not a procedure",
            LispError::ArityMismatch(_, _, _) => "arity mismatch",
            LispError::TypeMismatch(_, _) => "type mismatch",
        }
    }
}

// TODO 补充完整ASCII中所有的不可打印字符
// FIXME newline应该根据平台决定是linefeed还是return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
pub fn name_to_char<'a>(name: &'a str) -> Option<char> {
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
