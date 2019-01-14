use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;

use std::process::exit;

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

pub fn define(context: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity == 0 {
        return Err(BadSyntax("define".to_owned(), String::new()));
    }
    match exprs[0] {
        Sexp::Symbol(ref sym) => {
            if arity == 1 {
                Err(BadSyntax(
                    "define".to_owned(),
                    "(missing expression after identifier)".to_owned(),
                ))
            } else if arity > 2 {
                Err(BadSyntax(
                    "define".to_owned(),
                    "(multiple expressions after identifier)".to_owned(),
                ))
            } else {
                let val = context.eval(&exprs[1])?;
                context.define_variable(sym, val.clone());
                Ok(Sexp::Void)
            }
        }
        _ => Err(BadSyntax("define".to_owned(), String::new())),
    }
}

pub fn assign(_context: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity == 0 {
        return Err(BadSyntax("set!".to_owned(), String::new()));
    }
    Err(NotImplemented("set!".to_owned()))
}

pub fn quote(_context: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    let arity = exprs.len();
    if arity != 1 {
        return Err(BadSyntax("quote".to_owned(), "bad syntax".to_owned()));
    }
    Ok(exprs[0].clone())
}

pub fn lambda(context: &mut Context, exprs: Vec<Sexp>) -> LispResult {
    use scheme::types::Sexp::*;

    let arity = exprs.len();
    if arity == 0 {
        return Err(BadSyntax("lambda".to_owned(), "bad syntax\n in: (lambda)".to_owned()));
    } else if arity == 1 {
        return Err(BadSyntax("body".to_owned(), "no expression in body".to_owned()));
    }
    match exprs[0] {
        Symbol(ref sym) => {
            Ok(Closure {
                name: String::new(),
                params: vec![],
                vararg: Some(sym.clone()),
                body: exprs[1..].to_vec(),
                env: context.get_env().clone(),
            })
        }
        _ => Err(NotImplemented("lambda".to_owned()))
    }
}
