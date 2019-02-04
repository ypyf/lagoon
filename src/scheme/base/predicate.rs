use scheme::data::LispResult;
use scheme::data::error::LispError::*;
use scheme::data::value::Sexp;
use scheme::data::value::Sexp::*;

// pair?
// Predicates non-empty list
pub fn is_pair(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("pair?".to_string(), 1, arity));
    }
    Ok(Sexp::bool(args[0].is_pair()))
}

// number?
pub fn is_number(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("number?".to_string(), 1, arity));
    }

    if let Number(_) = args[0] {
        Ok(True)
    } else {
        Ok(False)
    }
}

pub fn is_integer(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("integer?".to_string(), 1, arity));
    }

    match args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_rational(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("rational?".to_string(), 1, arity));
    }

    match args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_real(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("real?".to_string(), 1, arity));
    }

    match args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_complex(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("complex?".to_string(), 1, arity));
    }

    match args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_exact(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("exact?".to_string(), 1, arity));
    }

    match args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_inexact(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("inexact?".to_string(), 1, arity));
    }

    match args[0] {
        Number(_) => Ok(False),
        _ => Ok(False),
    }
}

pub fn is_string(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("string?".to_string(), 1, arity));
    }

    match args[0] {
        Str(_, _) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_char(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("char?".to_string(), 1, arity));
    }

    match args[0] {
        Char(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_symbol(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("symbol?".to_string(), 1, arity));
    }
    Ok(Sexp::bool(args[0].is_symbol()))
}

pub fn is_procedure(args: &[Sexp]) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("procedure?".to_string(), 1, arity));
    }

    match args[0] {
        Procedure { .. } => Ok(True),
        Closure { .. } => Ok(True),
        _ => Ok(False),
    }
}


