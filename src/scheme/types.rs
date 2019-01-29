use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::str;
use std::cell::RefCell;
use scheme::data::iterator::{ListIterator, ListIntoIterator};

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
            List(xs) => {
                self.nest_level += 1;
                self.last_expr = Rc::new(expr.clone());
                let proc = self.eval(xs.first().unwrap())?;
                let ret = match proc {
                    Syntax { keyword, transformer } => self.apply_transformer(&keyword, transformer, expr.clone()),
                    _ => {
                        let args = expr.tail().unwrap().to_vec();
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
        let mut rules = transformer.rules.clone();
        for rule in &mut rules {
            println!("{}", rule.pattern);
            if let Some(bindings) = Context::match_syntax_rule(&rule.pattern, &form) {
//                dbg!(&bindings);
                let expr = self.render_template(&bindings, &rule.template);
                println!("{}", expr);
                return self.eval(&expr);
            }
        }
        self.syntax_error(keyword)
    }

    fn render_template(&mut self, bindings: &HashMap<String, Sexp>, template: &Sexp) -> Sexp {
        use self::Sexp::*;
        let mut q = VecDeque::new();
        let mut r: Sexp = Nil;
        q.push_back(template);
        loop {
            let list: &Sexp = if let Some(list) = q.pop_front() {
                list
            } else {
                return r;
            };

            for (index, item) in list.into_iter().enumerate() {
                if let Symbol(ident) = &item {
                    if let Some(val) = bindings.get(ident) {
                        r = list.clone();
                        r.set_nth(index, val);
                    }
                } else if item.is_pair() {
                    q.push_back(item)
                }
            }
            r = list.clone();
        }
    }

    fn match_syntax_rule(pat: &Sexp, form: &Sexp) -> Option<HashMap<String, Sexp>> {
        use self::Sexp::*;
        let mut q: VecDeque<(&Sexp, &Sexp)> = VecDeque::new();
        let mut bindings = HashMap::new();
        q.push_back((pat, form));
        loop {
            let (p, f): (&Sexp, &Sexp) = if let Some((a, b)) = q.pop_front() {
                (a, b)
            } else {
                return Some(bindings);
            };

            if p.is_list() != f.is_list() {
                return None;
            }

            let mut index_b = 0;
            for (index, a) in p.as_slice().unwrap().iter().enumerate() {
                if let Symbol(ident) = a {
                    if ident == ELLIPSIS {
                        if f.list_count() < p.list_count() - 2 {
                            return None;
                        }
                    } else if ident != UNDERSCORE {
                        if let Some(b) = f.nth(index) {
                            index_b += 1;
                            bindings.insert(ident.clone(), b.clone());
                            continue;
                        }
                        return None;
                    }
                } else if a.is_pair() {
                    if let Some(b) = f.nth(index) {
                        index_b += 1;
                        if b.is_pair() {
                            q.push_back((a, b));
                            continue;
                        }
                    }
                    return None;
                } else {
                    if let Some(b) = f.nth(index) {
                        index_b += 1;
                        if a == b {
                            continue;
                        }
                    }
                    return None;
                }
            }

            if index_b + 1 != f.list_count() {
                return None;
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
                            let mut xs = vals[nparams..].to_vec();
                            xs.push(Nil);
                            List(xs)
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
    List(Vec<Sexp>),
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

impl IntoIterator for Sexp {
    type Item = Sexp;
    type IntoIter = ListIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIntoIterator::new(self)
    }
}

impl<'a> IntoIterator for &'a Sexp {
    type Item = &'a Sexp;
    type IntoIter = ListIterator<'a>;

    // 注意这里的self是引用
    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self)
    }
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
        if let Sexp::List(_) = self {
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
            List(xs) => {
                *xs.last().unwrap() == Nil
            }
            _ => false,
        }
    }

    // 计算元素个数（不包括尾部的Nil, 不递归计算）
    pub fn list_count(&self) -> usize {
        use self::Sexp::*;
        if let List(xs) = self {
            if self.is_list() {
                xs.len() - 1
            } else {
                xs.len()
            }
        } else {
            panic!()
        }
    }

    pub fn init(&self) -> Option<&[Sexp]> {
        if let Sexp::List(xs) = self {
            let (_, init) = xs.split_last().unwrap();
            Some(init)
        } else {
            None
        }
    }

    pub fn tail(&self) -> Option<&[Sexp]> {
        if let Sexp::List(xs) = self {
            let (_, tail) = xs.split_first().unwrap();
            Some(tail)
        } else {
            None
        }
    }

    // 返回指定下标后的元素，不包括尾部的Nil
    pub fn rest(&self, from: usize) -> Option<&[Sexp]> {
        if let Sexp::List(xs) = self {
            if self.is_list() {
                Some(&xs[from..xs.len() - 1])
            } else {
                Some(&xs[from..])
            }
        } else {
            None
        }
    }

    // 返回列表中的第n个元素，不包含尾部的Nil
    pub fn nth(&self, n: usize) -> Option<&Sexp> {
        if let Sexp::List(xs) = self {
            if n >= self.list_count() {
                return None;
            }
            Some(&xs[n])
        } else {
            None
        }
    }

    // 返回列表，不包含尾部的Nil
    pub fn as_slice(&self) -> Option<&[Self]> {
        if let Sexp::List(xs) = self {
            if self.is_list() {
                let (_, init) = xs.split_last().unwrap();
                Some(init)
            } else {
                Some(xs)
            }
        } else {
            None
        }
    }

    pub fn set_nth(&mut self, n: usize, sexp: &Sexp) {
        if n >= self.list_count() {
            return;
        }
        if let Sexp::List(xs) = self {
            xs[n] = sexp.clone();
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
            (List(xs1), List(xs2)) => xs1 == xs2,
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
                write!(f, "#<procedure {}>", name)
            }
            Syntax { keyword, .. } => write!(f, "#<syntax:{}>", keyword),
            List(xs) => {
                let mut string = String::new();
                string.push('(');
                let (last, init) = xs.split_last().unwrap();
                for expr in init {
                    string.push_str(&format!("{} ", expr))
                }
                if *last == Nil {
                    // remove the last space
                    string.pop();
                } else {
                    string.push_str(&format!(". {}", last));
                }
                string.push(')');
                write!(f, "{}", string)
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
