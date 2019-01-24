use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::str;
use std::cell::RefCell;

pub const UNDERSCORE: &str = "_";

// 省略号默认为三点，可配置
pub const ELLIPSIS: &str = "...";

pub type LispResult<T> = Result<T, LispError>;

type Env = Vec<HashMap<String, Rc<RefCell<Sexp>>>>;

#[derive(Debug, Clone)]
pub struct Context {
    pub env: Env,
    last_expr: Rc<Sexp>,
    current_proc: Rc<Sexp>,
    // 当前apply的过程
    nest_level: i64,
}

#[allow(dead_code)]
impl Context {
    pub fn new() -> Self {
        Context {
            env: Env::new(),
            last_expr: Rc::new(Sexp::Void),
            current_proc: Rc::new(Sexp::Void),
            nest_level: 0,
        }
    }

    pub fn is_toplevel(&self) -> bool {
        self.nest_level == 0
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
        self.env.push(HashMap::new())
    }

    pub fn leave_scope(&mut self) {
        self.env.pop();
    }

    pub fn bind(&mut self, name: &str, val: &Sexp) {
        let var = Rc::new(RefCell::new(val.clone()));
        self.env.last_mut().unwrap().insert(name.to_owned(), var);
    }

    pub fn assign(&mut self, name: &str, val: &Sexp) -> bool {
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
        self.bind(name, &form);
    }

    pub fn def_proc(&mut self, name: &str, func: Function) {
        let proc = Sexp::Function {
            name: name.to_owned(),
            special: false,
            func,
        };
        self.bind(name, &proc);
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<RefCell<Sexp>>> {
        for current in self.env.iter().rev() {
            if let Some(val) = current.get(name) {
                return Some(val.clone());
            }
        }
        None
    }

    pub fn syntax_error<T>(&self, form: &str) -> LispResult<T> {
        Err(LispError::BadSyntax(form.to_string(), None))
    }

    pub fn syntax_error1<T>(&self, form: &str, reason: &str) -> LispResult<T> {
        Err(LispError::BadSyntax(form.to_string(), Some(reason.to_string())))
    }

    pub fn eval(&mut self, expr: &Sexp) -> LispResult<Sexp> {
        use self::Sexp::*;
        use self::LispError::*;

        match expr {
            Symbol(name) => match self.lookup(name.as_str()) {
                // 顶层求值syntax是语法错误
                Some(val) => if self.is_toplevel() {
                    match val.borrow().clone() {
                        Syntax { keyword, .. } => self.syntax_error(&keyword),
                        otherwise => Ok(otherwise)
                    }
                } else {
                    Ok(val.borrow().clone())
                }
                None => Err(Undefined(name.to_string())),
            },
            Nil => return Err(ApplyError("missing procedure expression".to_owned())),
            List(v, t) => {
                self.nest_level += 1;
                self.last_expr = Rc::new(expr.clone());
                if v.is_empty() {
                    return Err(ApplyError("missing procedure expression".to_owned()));
                }
                let proc = self.eval(&v[0])?;
                let ret = match proc {
                    Syntax { keyword, transformer } => self.apply_transformer(&keyword, transformer, expr.clone()),
                    _ => {
                        let mut args = v[1..].to_vec();
                        args.push((**t).clone());
                        self.apply(proc, args)
                    }
                };
                self.nest_level -= 1;
                ret
            }
            _ => Ok(expr.clone()), // 其它表达式求值到其本身
        }
    }

    fn apply_transformer(&mut self, keyword: &str, transformer: Transformer, form: Sexp) -> LispResult<Sexp> {
        for rule in transformer.rules {
            println!("match {} {}", rule.pattern, form);
            if let (idents, true) = Context::match_syntax_rule(keyword, &rule.pattern, &form) {
                self.enter_scope();
                for (ident, datum) in idents {
                    self.bind(&ident, &datum);
                }
                // 注意这里使用的ctx以及求值的顺序
                let res = self.eval(&rule.template)?;
                self.leave_scope();
                return self.eval(&res);
            }
        }
        self.syntax_error(keyword)
    }

    fn match_syntax_rule(keyword: &str, pat: &Sexp, form: &Sexp) -> (HashMap<String, Sexp>, bool) {
        use self::Sexp::*;
        let mut list_stack = vec![];
        let mut bindings = HashMap::new();
        let mut a = pat.clone();
        let mut b = form.clone();
        loop {
            match &a {
                // FIXME 判断literals
                Symbol(ident) => if ident != keyword && ident != ELLIPSIS {
                    bindings.insert(ident.clone(), b.clone());
                } else {
                    //bindings.insert(ident.clone(), current_b.clone());
                }
                List(init1, last1) => match &b {
                    List(init2, last2) => {
                        // FIXME 匹配可变参数和ellipsis
                        let mut pos = 0;    // 记录固定实参数的位置
                        for a in init1 {
                            match a {
                                Symbol(ident) => if ident == ELLIPSIS {
                                    list_stack.push((a.clone(), form.clone()))
                                } else {
                                    if init2.len() <= pos {
                                        return (bindings, false);
                                    }
                                    list_stack.push((a.clone(), init2[pos].clone()));
                                    pos += 1;
                                }
                                _ => {
                                    if init2.len() <= pos {
                                        return (bindings, false);
                                    }
                                    list_stack.push((a.clone(), init2[pos].clone()));
                                    pos += 1;
                                }
                            }
                        }
                    }
                    _ => return (bindings, false),
                },
                _ => if a != b { return (bindings, false); }
            }
            if list_stack.is_empty() {
                return (bindings, true);
            } else {
                let (a1, b1) = list_stack.pop().unwrap();
                a = a1;
                b = b1;
            }
        }
    }

    fn apply(&mut self, proc: Sexp, exprs: Vec<Sexp>) -> LispResult<Sexp> {
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
                        return self.syntax_error("apply");
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
                    return self.syntax_error("apply");
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
                    // FIXME expected least nparams...
                    return Err(ArityMismatch(func_name, nparams, nargs));
                } else if nargs != nparams && vararg.is_none() {
                    return Err(ArityMismatch(func_name, nparams, nargs));
                }

                let mut vals = Vec::with_capacity(args.len());
                for arg in &args {
                    let val = self.eval(arg)?;
                    vals.push(val);
                }

                match vararg {
                    Some(sym) => {
                        for (k, v) in params.iter().zip(vals.iter()) {
                            context.bind(k, v);
                        }
                        let ref val = if nargs == nparams {
                            Nil
                        } else {
                            List(vals[nparams..].to_vec(), Rc::new(Nil))
                        };
                        context.bind(&sym, val);
                    }
                    _ => {
                        for (k, v) in params.iter().zip(vals.iter()) {
                            context.bind(k, v);
                        }
                    }
                }
                // FIXME tail call optimization
                let (last, elements) = body.split_last().unwrap();
                for expr in elements {
                    context.eval(&expr).unwrap();
                }
                context.eval(last)
            }
            _ => Err(ApplyError(format!("not a procedure: {}", proc))),
        }
    }
}

pub type Function = fn(&mut Context, Vec<Sexp>) -> LispResult<Sexp>;

#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pub pattern: Sexp,
    pub template: Sexp,
}

#[derive(Debug, Clone)]
pub struct Transformer {
    pub id: Option<String>,
    pub literals: Vec<String>,
    pub rules: Vec<SyntaxRule>,
}

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
    Syntax {
        keyword: String,
        transformer: Transformer,
    },
}

impl Sexp {
    pub fn is_true(&self) -> bool {
        // Scheme中只有#f是假值
        if let Sexp::False = self {
            false
        } else {
            true
        }
    }

    pub fn bool(b: bool) -> Sexp {
        if b {
            Sexp::True
        } else {
            Sexp::False
        }
    }

    // null?
    pub fn is_null(&self) -> bool {
        if let Sexp::Nil = self {
            true
        } else {
            false
        }
    }

    // pair?
    pub fn is_pair(&self) -> bool {
        if let Sexp::List(_, _) = self {
            true
        } else {
            false
        }
    }

    // list?
    pub fn is_list(&self) -> bool {
        use self::Sexp::*;
        match self {
            Nil => true,
            List(_, last) => match **last {
                Nil => true,
                _ => false,
            }
            _ => false,
        }
    }

    pub fn is_improper_list(&self) -> bool {
        use self::Sexp::*;
        match self {
            List(_, last) => match **last {
                Nil => false,
                _ => true,
            }
            _ => false,
        }
    }

    // 计算元素个数（不包括尾部的Nil, 不递归计算）
    pub fn count(&self) -> usize {
        use self::Sexp::*;
        if let List(init, last) = self {
            if **last != Nil {
                init.len() + 1
            } else {
                init.len()
            }
        } else {
            1
        }
    }

    pub fn to_vec(&self) -> Vec<Self> {
        use self::Sexp::*;
        match self {
            Nil => vec![],
            List(init, last) => if **last == Nil {
                init.clone()
            } else {
                let mut res = init.clone();
                res.push((**last).clone());
                res
            }
            _ => vec![self.clone()],
        }
    }

    pub fn first(&self) -> &Self {
        use self::Sexp::*;
        if let List(init, _) = self {
            &init[0]
        } else {
            self
        }
    }

    pub fn first_mut(&mut self) -> &mut Self {
        use self::Sexp::*;
        if let List(init, _) = self {
            &mut init[0]
        } else {
            self
        }
    }

    pub fn is_symbol(&self) -> bool {
        if let Sexp::Symbol(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_mutable_string(&self) -> bool {
        if let Sexp::Str(_, true) = self {
            true
        } else {
            false
        }
    }

    pub fn is_immutable_string(&self) -> bool {
        if let Sexp::Str(_, false) = self {
            true
        } else {
            false
        }
    }
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
            Closure { name, .. } => if name.is_empty() {
                write!(f, "#<procedure>")
            } else {
                write!(f, "#<procedure:{}>", name)
            }
            Syntax { keyword, .. } => write!(f, "#<syntax:{}>", keyword),
            List(first, second) => {
                let mut datum = String::with_capacity(first.len() + 2);
                datum.push('(');
                for expr in first.iter() {
                    datum.push_str(format!("{} ", expr).as_str());
                }

                let mut exprs = second.clone();
                loop {
                    match *exprs {
                        Nil => break,
                        List(ref init, ref last) => {
                            for expr in init.iter() {
                                datum.push_str(format!("{} ", expr).as_str());
                            }
                            exprs = last.clone();
                        }
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
    DivisionByZero(String),
    ReadError(String),
    AssignError(String, Context),
    BadSyntax(String, Option<String>),
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
            DivisionByZero(sym) => write!(f, "Error in {}: division by zero", sym),
            ReadError(err) => write!(f, "read: {}", err),
            AssignError(err, _ctx) => write!(f, "set!: {}", err),
            BadSyntax(sym, err) => {
                let msg = if let Some(reason) = err {
                    reason.clone()
                } else {
                    "bad syntax".to_owned()
                };
                write!(f, "{}: {}", sym, msg)
            }
            IndexOutOfRange(sym, index, lower, upper) =>
                write!(f, "{}: index is out of range\n index: {}\n valid range: [{}, {}]", sym, index, lower, upper),
            Undefined(sym) => write!(f, "Error: variable {} is not bound", sym),
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
            DivisionByZero(_) => "division by zero",
            ReadError(_) => "read error",
            AssignError(_, _) => "set! error",
            BadSyntax(_, _) => "bad syntax",
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
