use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::*;

// Predicate non-empty list
pub fn is_pair(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("pair?".to_string(), 1, arity));
    }

    match &args[0] {
        Nil => Ok(False),
        List(_, _) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_number(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("number?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_integer(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("integer?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_rational(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("rational?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_real(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("real?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_complex(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("complex?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_exact(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("exact?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_inexact(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("inexact?".to_string(), 1, arity));
    }

    match &args[0] {
        Number(_) => Ok(False),
        _ => Ok(False),
    }
}

pub fn is_string(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("string?".to_string(), 1, arity));
    }

    match &args[0] {
        Str(_, _) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_char(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("char?".to_string(), 1, arity));
    }

    match &args[0] {
        Char(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_symbol(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("symbol?".to_string(), 1, arity));
    }

    match &args[0] {
        Symbol(_) => Ok(True),
        _ => Ok(False),
    }
}

pub fn is_procedure(_context: &mut Context, args: Vec<Sexp>) -> LispResult {
    let arity = args.len();
    if arity == 0 {
        return Err(ArityMismatch("procedure?".to_string(), 1, arity));
    }

    match &args[0] {
        Function { .. } => Ok(True),
        Closure { .. } => Ok(True),
        _ => Ok(False),
    }
}

