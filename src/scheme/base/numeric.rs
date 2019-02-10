extern crate itertools;

use scheme::LispResult;
use scheme::error::LispError::*;
use scheme::value::Sexp;
use scheme::value::Sexp::{Number, True, False};

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

pub fn arith_op(operator: Operator, args: &[Sexp]) -> LispResult<Sexp> {
    use self::Operator::*;
    let coll: Result<Vec<_>, _> = args.iter().map(|arg| if let Number(n) = arg { Ok(*n) } else { Err(arg) }).collect();
    if coll.is_err() {
        return Err(TypeMismatch("number".to_owned(), format!("{}", coll.unwrap_err())));
    }
    let nums = coll.unwrap();
    let res = match operator {
        Add => nums.iter().fold(0, ops::Add::add),
        Sub => {
            if nums.len() == 0 {
                return Err(ArityMismatch(operator.to_string(), 1, 0));
            }
            nums.into_iter().fold1(ops::Sub::sub).unwrap()
        }
        Mul => nums.iter().fold(1, ops::Mul::mul),
        Div => {
            if nums.len() == 0 {
                return Err(ArityMismatch(operator.to_string(), 1, 0));
            }
            if nums[1..].iter().map(|&x| x == 0).fold(false, |acc, b| acc || b) {
                return Err(DivisionByZero("/".to_owned()));
            }
            nums.into_iter().fold1(ops::Div::div).unwrap()
        }
    };
    Ok(Number(res))
}

pub fn compare<F>(name: &str, operator: F, args: &[Sexp]) -> LispResult<Sexp> where
    F: Fn(i64, i64) -> bool {
    let arity = args.len();
    if arity < 2 {
        return Err(ArityMismatch(name.to_string(), 2, arity));
    }

    let coll: Result<Vec<_>, _> = args.iter().map(|arg| if let Number(n) = arg { Ok(*n) } else { Err(arg) }).collect();
    if coll.is_err() {
        return Err(TypeMismatch("number".to_owned(), format!("{}", coll.unwrap_err())));
    }
    let nums = coll.unwrap();

    let mut acc = true;
    for i in 0..nums.len() - 1 {
        acc = acc && operator(nums[i], nums[i + 1])
    }

    if acc {
        Ok(True)
    } else {
        Ok(False)
    }
}
