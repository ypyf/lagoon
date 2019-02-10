use std::rc::Rc;

use scheme::LispResult;
use scheme::value::Sexp;
use scheme::value::Sexp::*;
use scheme::error::LispError::*;
use scheme::context::Context;

pub fn quote(_context: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 1 {
        return Err(BadSyntax("quote".to_string()));
    }
    Ok(exprs[0].clone())
}

// FIXME (define a (define a 1)) => define: not allowed in an expression context
// definition in expression context, where definitions are not allowed, in from (define a 1)
pub fn define(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity == 0 {
        syntax_error!("define", "bad syntax");
    } else if arity == 1 {
        syntax_error!("define", "missing expression after identifier");
    } else if arity > 2 {
        syntax_error!("define", "multiple expressions after identifier");
    }

    match &exprs[0] {
        Symbol(sym) => {
            let val = ctx.eval(&exprs[1])?;
            match val {
                Closure { name: _, params, vararg, body, env } => {
                    let closure = Closure { name: sym.clone(), params, vararg, body, env };
                    ctx.insert(sym, &closure);
                }
                _ => ctx.insert(sym, &val)
            }
        }
        List(xs) => match xs.last().unwrap() {
            // (define (name x y) (+ x y)) => (define name (lambda (x y) (+ x y)))
            Nil => {
                let expr = if let Some((name, params)) = xs.split_first() {
                    let params = List(Box::new(params.to_vec()));
                    let lambda = List(Box::new(vec![Symbol("lambda".to_owned()), params, exprs[1].clone(), Nil]));
                    List(Box::new(vec![Symbol("define".to_owned()), name.clone(), lambda, Nil]))
                } else {
                    syntax_error!("define", "bad syntax");
                };
                ctx.eval(&expr)?;
            }
            // (define (name x y . v) (+ x y)) => (define name (lambda (x y . v) (+ x y)))
            Symbol(_) => {
                let expr = if let Some((name, params)) = xs.split_first() {
                    let params = List(Box::new(params.to_vec()));
                    let lambda = List(Box::new(vec![Symbol("lambda".to_owned()), params, exprs[1].clone(), Nil]));
                    List(Box::new(vec![Symbol("define".to_owned()), name.clone(), lambda, Nil]))
                } else {
                    syntax_error!("define", "bad syntax");
                };
                ctx.eval(&expr)?;
            }
            _ => syntax_error!("define", "bad syntax"),
        }
        _ => syntax_error!("define", "bad syntax"),
    }
    Ok(Void)
}

pub fn assign(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 2 {
        syntax_error!("set!", format!("has {} parts after keyword", arity));
    }

    match &exprs[0] {
        Symbol(sym) => {
            let val = ctx.eval(&exprs[1])?;
            if ctx.assign(sym, &val) {
                Ok(Void)
            } else {
                return Err(AssignError("cannot set undefined".to_owned()));
            }
        }
        _ => return Err(AssignError("not an identifier".to_owned())),
    }
}

// FIXME body不能为空
pub fn lambda(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity == 0 {
        syntax_error!("lambda", "bad syntax");
    } else if arity == 1 {
        syntax_error!("body", "no expression in body");
    }

    let (formals, body) = exprs.split_first().unwrap();
    match formals {
        Nil => Ok(Closure {
            name: String::new(),
            params: vec![],
            vararg: None,
            body: Rc::new(body.to_vec()),
            env: ctx.clone(),
        }),
        List(xs) => {
            let (last, init) = xs.split_last().unwrap();
            let vararg = match last {
                Nil => None,
                Symbol(ident) => Some(ident.clone()),
                _ => syntax_error!("lambda", "not an identifier"),
            };

            let mut params = vec![];
            for expr in init {
                match expr {
                    Symbol(ident) => params.push(ident.clone()),
                    _ => syntax_error!("lambda", "not an identifier"),
                }
            }
            Ok(Closure {
                name: String::new(),
                params,
                vararg,
                body: Rc::new(body.to_vec()),
                env: ctx.clone(),
            })
        }
        Symbol(ident) => {
            Ok(Closure {
                name: String::new(),
                params: vec![],
                vararg: Some(ident.clone()),
                body: Rc::new(body.to_vec()),
                env: ctx.clone(),
            })
        }
        _ => syntax_error!("lambda", "not an identifier"),
    }
}

// FIXME tail call optimization
pub fn if_exp(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity < 2 || arity > 3 {
        syntax_error!("if", "bad syntax");
    }

    if arity == 2 {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        if ctx.eval(pred)?.is_true() {
            ctx.eval(conseq)
        } else {
            Ok(Void)
        }
    } else {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        let alt = &exprs[2];
        if ctx.eval(pred)?.is_true() {
            ctx.eval(conseq)
        } else {
            ctx.eval(alt)
        }
    }
}
