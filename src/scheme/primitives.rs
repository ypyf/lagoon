use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;

use std::process::exit;

fn syntax_error(syntax: &str, expr: &str) -> LispResult {
    Err(BadSyntax(syntax.to_owned(), format!("bad syntax\n in: {}", expr)))
}

pub fn plus(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Sexp::Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }
    let sum: i64 = vals.iter().fold(0, |acc, &x| acc + x);
    Ok(Sexp::Number(sum))
}

pub fn subtract(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("-".to_owned(), 1, arity));
    }

    let mut vals = Vec::with_capacity(arity);
    for arg in args {
        match arg {
            Sexp::Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }

    if arity == 1 {
        Ok(Sexp::Number(-vals[0]))
    } else {
        let mut acc = vals[0];
        for x in &vals[1..] {
            acc -= *x
        }
        Ok(Sexp::Number(acc))
    }
}

pub fn multiply(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Sexp::Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }
    let sum: i64 = vals.iter().fold(1, |acc, &x| acc * x);
    Ok(Sexp::Number(sum))
}

pub fn quit(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    for arg in args {
        match arg {
            Sexp::Number(n) => exit(n as i32),
            _ => exit(0),
        }
    }
    exit(0);
}

pub fn define(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity == 0 {
        return syntax_error("define", &ctx.get_current_expr().to_string());
    }
    match exprs[0] {
        Sexp::Symbol(ref sym) => {
            if arity == 1 {
                syntax_error("define", "(missing expression after identifier)")
            } else if arity > 2 {
                syntax_error("define", "(multiple expressions after identifier)")
            } else {
                let val = ctx.eval(&exprs[1])?;
                match val {
                    Sexp::Closure { name: _, params, vararg, body, context } => {
                        let closure = Sexp::Closure { name: sym.clone(), params, vararg, body, context };
                        ctx.define_variable(sym, closure);
                    }
                    _ => ctx.define_variable(sym, val.clone())
                }
                Ok(Sexp::Void)
            }
        }
        _ => syntax_error("define", ""),
    }
}

pub fn assign(_context: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity == 0 {
        return syntax_error("set!", "");
    }
    Err(NotImplemented("set!".to_owned()))
}

pub fn quote(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity != 1 {
        return syntax_error("quote", &ctx.get_current_expr().to_string());
    }
    Ok(exprs[0].clone())
}

pub fn lambda(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    use scheme::types::Sexp::*;

    let arity = exprs.len();
    if arity == 0 {
        return syntax_error("lambda", &ctx.get_current_expr().to_string());
    } else if arity == 1 {
        return syntax_error("body", "no expression in body");
    }

    match exprs[0] {
        Nil => Ok(Closure {
            name: String::new(),
            params: vec![],
            vararg: None,
            body: exprs[1..].to_vec(),
            context: (*ctx).clone(),
        }),
        List(ref init, ref last) => {
            let vararg = match (*(*last)).clone() {
                Nil => None,
                Symbol(sym) => Some(sym.clone()),
                _ => return syntax_error("lambda", "not an identifier"),
            };
            let mut params = vec![];
            for expr in init {
                match expr {
                    Symbol(sym) => params.push(sym.clone()),
                    _ => return syntax_error("lambda", "not an identifier"),
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
        Symbol(ref sym) => {
            Ok(Closure {
                name: String::new(),
                params: vec![],
                vararg: Some(sym.clone()),
                body: exprs[1..].to_vec(),
                context: (*ctx).clone(),
            })
        }
        _ => return syntax_error("lambda", "not an identifier"),
    }
}
