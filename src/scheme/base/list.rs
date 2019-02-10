use scheme::LispResult;
use scheme::error::LispError::*;
use scheme::value::Sexp;
use scheme::value::Sexp::*;

pub fn car(args: &[Sexp]) -> LispResult<Sexp> {
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

pub fn cdr(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("cdr".to_owned(), 1, arity));
    }
    if let List(xs) = &args[0] {
        let (_, tail) = xs.split_first().unwrap();
        if tail.len() == 1 {
            Ok(tail[0].clone())
        } else {
            Ok(List(Box::new(tail.to_vec())))
        }
    } else {
        Err(TypeMismatch("pair".to_owned(), format!("{}", args[0])))
    }
}

pub fn cons(args: &[Sexp]) -> LispResult<Sexp> {
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
        Ok(List(Box::new(vec![a, b])))
    }
}
