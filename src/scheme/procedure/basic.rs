use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::*;

use std::rc::Rc;

pub fn quote(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity != 1 {
        return Err(BadSyntax("quote".to_owned(), None, ctx.clone()));
    }
    Ok(exprs[0].clone())
}

pub fn define(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity == 0 {
        return Err(BadSyntax("define".to_owned(), None, ctx.clone()));
    } else if arity == 1 {
        return Err(BadSyntax("define".to_owned(), Some("missing expression after identifier".to_owned()), ctx.clone()));
    } else if arity > 2 {
        return Err(BadSyntax("define".to_owned(), Some("multiple expressions after identifier".to_owned()), ctx.clone()));
    }

    match &exprs[0] {
        Symbol(sym) => {
            let val = ctx.eval(&exprs[1])?;
            match val {
                Closure { name: _, params, vararg, body, context } => {
                    let closure = Closure { name: sym.clone(), params, vararg, body, context };
                    ctx.define_variable(sym, &closure);
                }
                _ => ctx.define_variable(sym, &val)
            }
            Ok(Void)
        }
        List(init, last) => {
            match **last {
                // (define (name x y) (+ x y)) => (define name (lambda (x y) (+ x y)))
                Nil => {
                    let expr = if let Some((name, params)) = init.split_first() {
                        let param_list = List(params.to_vec(), Rc::new(Nil));
                        let lambda = List(vec![Symbol("lambda".to_owned()), param_list, exprs[1].clone()], Rc::new(Nil));
                        List(vec![Symbol("define".to_owned()), name.clone(), lambda], Rc::new(Nil))
                    } else {
                        return Err(BadSyntax("define".to_owned(), None, ctx.clone()));
                    };
                    ctx.eval(&expr)
                }
                // (define (name x y . v) (+ x y)) => (define name (lambda (x y . v) (+ x y)))
                Symbol(_) => {
                    let expr = if let Some((name, params)) = init.split_first() {
                        let param_list = List(params.to_vec(), last.clone());
                        let lambda = List(vec![Symbol("lambda".to_owned()), param_list, exprs[1].clone()], Rc::new(Nil));
                        List(vec![Symbol("define".to_owned()), name.clone(), lambda], Rc::new(Nil))
                    } else {
                        return Err(BadSyntax("define".to_owned(), None, ctx.clone()));
                    };
                    ctx.eval(&expr)
                }
                _ => Err(BadSyntax("define".to_owned(), None, ctx.clone())),
            }
        }
        _ => Err(BadSyntax("define".to_owned(), None, ctx.clone())),
    }
}

pub fn assign(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity != 2 {
        return Err(BadSyntax("set!".to_owned(), Some(format!("has {} parts after keyword", arity)), ctx.clone()));
    }

    match &exprs[0] {
        Symbol(sym) => {
            let val = ctx.eval(&exprs[1])?;
            if ctx.set_variable(sym, &val) {
                Ok(Void)
            } else {
                return Err(AssignError("cannot set undefined".to_owned(), ctx.clone()));
            }
        }
        _ => return Err(AssignError("not an identifier".to_owned(), ctx.clone())),
    }
}

pub fn lambda(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity == 0 {
        return Err(BadSyntax("lambda".to_owned(), None, ctx.clone()));
    } else if arity == 1 {
        return Err(BadSyntax("body".to_owned(), Some("no expression in body".to_owned()), ctx.clone()));
    }

    match &exprs[0] {
        Nil => Ok(Closure {
            name: String::new(),
            params: vec![],
            vararg: None,
            body: exprs[1..].to_vec(),
            context: (*ctx).clone(),
        }),
        List(init, last) => {
            let vararg = match **last {
                Nil => None,
                Symbol(ref sym) => Some(sym.clone()),
                _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()), ctx.clone())),
            };
            let mut params = vec![];
            for expr in init {
                match expr {
                    Symbol(sym) => params.push(sym.clone()),
                    _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()), ctx.clone())),
                }
            }
            Ok(Closure {
                name: String::new(),
                params,
                vararg,
                body: exprs[1..].to_vec(),
                context: (*ctx).clone(),
            })
        }
        Symbol(sym) => {
            Ok(Closure {
                name: String::new(),
                params: vec![],
                vararg: Some(sym.clone()),
                body: exprs[1..].to_vec(),
                context: (*ctx).clone(),
            })
        }
        _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()), ctx.clone())),
    }
}

pub fn if_exp(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity < 2 || arity > 3 {
        return Err(BadSyntax("if".to_owned(), None, ctx.clone()));
    }

    if arity == 2 {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        match ctx.eval(pred) {
            // 只有False是假值
            Ok(False) => Ok(Void),
            Ok(_) => ctx.eval(conseq),  // FIXME 尾部
            Err(err) => Err(err),
        }
    } else {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        let alt = &exprs[2];
        match ctx.eval(pred) {
            // 只有False是假值
            Ok(False) => ctx.eval(alt), // FIXME 尾部
            Ok(_) => ctx.eval(conseq),  // FIXME 尾部
            Err(err) => Err(err),
        }
    }
}
