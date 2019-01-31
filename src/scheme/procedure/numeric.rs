extern crate itertools;

use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::{Number, True, False};

use std::fmt;
use std::ops;

use self::itertools::Itertools;

pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Operator::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
        }
    }
}

pub fn arith_op(op: Operator, _context: &mut Context, args: &[Sexp]) -> LispResult<Sexp> {
    use self::Operator::*;
    // 检查参数类型
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Number(n) => vals.push(*n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }
    let arity = vals.len();
    let res = match op {
        Add => vals.into_iter().fold(0, ops::Add::add),
        Sub => {
            if arity == 0 {
                return Err(ArityMismatch(op.to_string(), 1, arity));
            }
            vals.into_iter().fold1(ops::Sub::sub).unwrap()
        }
        Mul => vals.into_iter().fold(1, ops::Mul::mul),
        Div => {
            if arity == 0 {
                return Err(ArityMismatch(op.to_string(), 1, arity));
            }
            if vals[1..].iter().map(|&x| x == 0).fold(false, |acc, b| acc || b) {
                return Err(DivisionByZero("/".to_owned()));
            }
            vals.into_iter().fold1(ops::Div::div).unwrap()
        }
    };
    Ok(Number(res))
}

pub fn compare<F>(name: &str, op: F, _context: &mut Context, args: &[Sexp]) -> LispResult<Sexp> where
    F: Fn(i64, i64) -> bool {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Number(n) => vals.push(*n),
            _ => return Err(TypeMismatch("number".to_owned(), format!("{}", arg))),
        }
    }

    let arity = vals.len();
    if arity < 2 {
        return Err(ArityMismatch(name.to_string(), 2, arity));
    }

    let mut acc = true;
    for i in 0..vals.len() - 1 {
        acc = acc && op(vals[i], vals[i + 1])
    }

    if acc {
        Ok(True)
    } else {
        Ok(False)
    }
}
