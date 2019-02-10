use std::rc::Rc;

use scheme::LispResult;
use scheme::value::Sexp;
use scheme::value::Sexp::*;
use scheme::env::Environment;
use scheme::error::LispError::*;
use scheme::value::HostFunction2;
use scheme::value::Transformer;
use std::collections::HashMap;
use std::collections::vec_deque::VecDeque;
use scheme::ELLIPSIS;
use scheme::UNDERSCORE;


pub struct LispMachine {
    pub exp: Option<Sexp>,
    pub env: Environment,
    pub val: Option<Sexp>,
    pub argl: Vec<Sexp>,
    pub unev: Option<Sexp>,
}

impl LispMachine {
    pub fn new(env: Environment) -> Self {
        LispMachine {
            exp: None,
            env,
            val: None,
            argl: vec![],
            unev: None,
        }
    }

    pub fn eval(&mut self, expr: &Sexp) -> LispResult<Sexp> {
        match expr {
            Nil => return Err(ApplyError("missing procedure expression".to_owned())),
            Symbol(name) => if let Some(val) = self.env.lookup(&name) {
                let val = val.borrow().clone();
                match &val {
                    // 语法关键字如果不位于列表头部是无效的
                    Syntax { name, .. } => syntax_error!(name, "bad syntax"),
                    DefineSyntax { keyword, .. } => syntax_error!(keyword, "bad syntax"),
                    _ => Ok(val)
                }
            } else {
                Err(Undefined(name.to_string()))
            }
            List(xs) => self.eval_list(&*xs),
            _ => Ok(expr.clone()),
        }
    }

    // 对列表中的第一个元素求值
    fn eval_head(&mut self, expr: &Sexp) -> LispResult<Sexp> {
        use self::Sexp::*;
        match expr {
            Symbol(name) => if let Some(val) = self.env.lookup(&name) {
                Ok(val.borrow().clone())
            } else {
                Err(Undefined(name.to_string()))
            }
            _ => self.eval(expr),
        }
    }

    // 对列表求值
    fn eval_list(&mut self, exprs: &[Sexp]) -> LispResult<Sexp> {
        use self::Sexp::*;

        let (head, tail) = exprs.split_first().unwrap();
        let proc = self.eval_head(head)?;
        match proc {
            DefineSyntax { keyword, transformer } => {
                let expr = List(Box::new(exprs.to_vec()));
                self.apply_transformer(&keyword, transformer, &expr)
            }
            Syntax { func, .. } => self.apply_keyword(func, tail),
            _ => {
                let (last, init) = tail.split_last().unwrap();
                if *last != Nil {
                    syntax_error!("apply", "bad syntax")
                }
                let args: Result<Vec<_>, _> = init.iter().map(|e| self.eval(e)).collect();
                if args.is_err() {
                    return Err(args.unwrap_err());
                }
                self.apply(&proc, &args.unwrap())
            }
        }
    }

    pub fn apply(&mut self, proc: &Sexp, exprs: &[Sexp]) -> LispResult<Sexp> {
        use self::Sexp::*;

        match proc {
            Function { func, .. } => func(exprs),
            Procedure { func, .. } => func(self, exprs),
            Closure { name, params, vararg, body, env } => {
                let func_name = if name.is_empty() {
                    "#<procedure>"
                } else {
                    name
                };

                let args = exprs;
                let nparams = params.len();
                let nargs = args.len();
                let mut ctx = Environment::new(Some(Rc::new(env.clone())));
                match vararg {
                    Some(ref name) => {
                        if nargs < nparams {
                            // FIXME expected least nparams...
                            return Err(ArityMismatch(func_name.to_string(), nparams, nargs));
                        }
                        let (pos, rest) = args.split_at(nparams);
                        ctx.insert_many(params, pos);
                        let varg = if nargs == nparams {
                            Nil
                        } else {
                            let mut xs = rest.to_vec();
                            xs.push(Nil);
                            List(Box::new(xs))
                        };
                        ctx.insert(name, &varg);
                    }
                    _ => {
                        if nargs != nparams {
                            return Err(ArityMismatch(func_name.to_string(), nparams, nargs));
                        }
                        ctx.insert_many(params, &args);
                    }
                }
                let old_ctx = self.env.clone();
                self.env = ctx;
                // FIXME tail call optimization
                let (last, init) = body.split_last().unwrap();
                for expr in init {
                    self.eval(expr)?;
                }
                let res = self.eval(last);
                self.env = old_ctx;
                res
            }
            _ => Err(ApplyError(format!("not a procedure: {}", proc))),
        }
    }

    fn apply_transformer(&mut self, keyword: &str, transformer: Transformer, form: &Sexp) -> LispResult<Sexp> {
        let mut rules = transformer.rules.clone();
        for rule in &mut rules {
            println!("{}", rule.pattern);
            if let Some(bindings) = Self::match_syntax_rule(&rule.pattern, form) {
                // dbg!(&bindings);
                let expr = self.render_template(&bindings, &rule.template);
                println!("{}", expr);
                return self.eval(&expr);
            }
        }
        syntax_error!(keyword, "bad syntax")
    }

    fn apply_keyword(&mut self, func: HostFunction2, exprs: &[Sexp]) -> LispResult<Sexp> {
        use self::Sexp::*;
        let (last, init) = exprs.split_last().unwrap();
        let args = if *last == Nil {
            init
        } else {
            exprs
        };
        func(self, args)
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
}

