use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::*;

pub fn plus(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }
    let sum: i64 = vals.iter().fold(0, |acc, &x| acc + x);
    Ok(Number(sum))
}

pub fn subtract(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("-".to_owned(), 1, arity));
    }

    let mut vals = Vec::with_capacity(arity);
    for arg in args {
        match arg {
            Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }

    if arity == 1 {
        Ok(Number(-vals[0]))
    } else {
        let mut acc = vals[0];
        for x in &vals[1..] {
            acc -= *x
        }
        Ok(Number(acc))
    }
}

pub fn multiply(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }
    let sum: i64 = vals.iter().fold(1, |acc, &x| acc * x);
    Ok(Number(sum))
}

pub fn division(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("/".to_owned(), 1, arity));
    }

    let mut vals = Vec::with_capacity(arity);
    for arg in args {
        match arg {
            Number(n) => vals.push(n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }

    if arity == 1 {
        Ok(Number(1/vals[0]))
    } else {
        let mut acc = vals[0];
        for x in &vals[1..] {
            acc /= *x
        }
        Ok(Number(acc))
    }
}
