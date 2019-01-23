use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::*;
use std::rc::Rc;

pub fn car(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("car".to_owned(), 1, arity));
    }
    match args[0] {
        List(ref init, _) => Ok(init[0].clone()),
        _ => Err(TypeMismatch("pair".to_owned(), format!("{}", args[0]))),
    }
}

pub fn cdr(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("cdr".to_owned(), 1, arity));
    }
    match &args[0] {
        List(init, last) => {
            let expr = List(init[1..].to_vec(), last.clone());
            Ok(expr)
        }
        _ => Err(TypeMismatch("pair".to_owned(), format!("{}", args[0]))),
    }
}

pub fn cons(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 2 {
        return Err(ArityMismatch("cons".to_owned(), 2, arity));
    }
    let a = &args[0];
    let b = &args[1];
    match b {
        List(init, last) => {
            let mut v = init.clone();
            v.insert(0, a.clone());
            Ok(List(v, last.clone()))
        }
        _ => Ok(List(vec![(*a).clone()], Rc::new(b.clone()))),
    }
}
