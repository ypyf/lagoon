use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::*;

pub fn car(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("car".to_owned(), 1, arity));
    }
    if let List(xs) = &args[0] {
        Ok(xs.first().unwrap().clone())
    } else {
        Err(TypeMismatch("pair".to_owned(), format!("{}", args[0])))
    }
}

pub fn cdr(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("cdr".to_owned(), 1, arity));
    }
    if args[0].is_pair() {
        let tail = args[0].tail().unwrap();
        Ok(List(tail.to_vec()))
    } else {
        Err(TypeMismatch("pair".to_owned(), format!("{}", args[0])))
    }
}

pub fn cons(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 2 {
        return Err(ArityMismatch("cons".to_owned(), 2, arity));
    }
    let a = args[0].clone();
    let mut b = args[1].clone();
    if let List(xs) = &mut b {
        xs.insert(0, a);
        Ok(List(xs.clone()))
    } else {
        Ok(List(vec![a, b]))
    }
}
