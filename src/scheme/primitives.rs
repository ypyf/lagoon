use scheme::types::Context;
use scheme::types::LispError;
use scheme::types::LispObject;
use scheme::types::Sexp;

pub fn plus(_context: &mut Context, args: &[Sexp]) -> LispObject {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Sexp::Number(n) => vals.push(n),
            _ => {
                return Err(LispError::TypeMismatch(
                    "number".to_owned(),
                    format!("{}", arg),
                ))
            }
        }
    }
    let sum: i64 = vals.iter().fold(0, |acc, &x| acc + x);
    Ok(Sexp::Number(sum))
}

pub fn subtract(_context: &mut Context, args: &[Sexp]) -> LispObject {
    let arity = args.len();
    if arity == 0 {
        return Err(LispError::ArityMismatch("-".to_owned(), 1, arity));
    }

    let mut vals = Vec::with_capacity(arity);
    for arg in args {
        match arg {
            Sexp::Number(n) => vals.push(n),
            _ => {
                return Err(LispError::TypeMismatch(
                    "number".to_owned(),
                    format!("{}", arg),
                ))
            }
        }
    }

    if arity == 1 {
        Ok(Sexp::Number(-vals[0]))
    } else {
        let mut acc = *vals[0];
        for x in &vals[1..] {
            acc -= *x
        }
        Ok(Sexp::Number(acc))
    }
}

pub fn mul(_context: &mut Context, args: &[Sexp]) -> LispObject {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Sexp::Number(n) => vals.push(n),
            _ => {
                return Err(LispError::TypeMismatch(
                    "number".to_owned(),
                    format!("{}", arg),
                ))
            }
        }
    }
    let sum: i64 = vals.iter().fold(1, |acc, &x| acc * x);
    Ok(Sexp::Number(sum))
}

pub fn define(context: &mut Context, exprs: &[Sexp]) -> LispObject {
    let arity = exprs.len();
    if arity == 0 {
        return Err(LispError::BadSyntax("define".to_owned(), String::new()));
    }
    match exprs[0] {
        Sexp::Symbol(ref sym) => {
            if arity == 1 {
                Err(LispError::BadSyntax(
                    "define".to_owned(),
                    "(missing expression after identifier)".to_owned(),
                ))
            } else if arity > 2 {
                Err(LispError::BadSyntax(
                    "define".to_owned(),
                    "(multiple expressions after identifier)".to_owned(),
                ))
            } else {
                let val = context.eval(&exprs[1])?;
                context.define_variable(sym, val);
                Ok(Sexp::Void)
            }
        }
        _ => Err(LispError::BadSyntax("define".to_owned(), String::new())),
    }
}

pub fn quote(_context: &mut Context, exprs: &[Sexp]) -> LispObject {
    let arity = exprs.len();
    if arity != 1 {
        return Err(LispError::BadSyntax("define".to_owned(), String::new()));
    }

    Ok(exprs[0].clone())
}
