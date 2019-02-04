use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum LispError {
    EndOfInput,
    Interrupted,
    DivisionByZero(String),
    ReadError(String),
    AssignError(String),
    BadSyntax(String, Option<String>),
    IndexOutOfRange(String, usize, usize, usize),
    Undefined(String),
    SystemError(String),
    ApplyError(String),
    ArityMismatch(String, usize, usize),
    TypeMismatch(String, String),
}

impl<'a> PartialEq for LispError {
    fn eq(&self, other: &LispError) -> bool {
        use self::LispError::*;

        match (self, other) {
            (EndOfInput, EndOfInput) => true,
            (Interrupted, Interrupted) => true,
            (ReadError(_), ReadError(_)) => true,
            (AssignError(_), AssignError(_)) => true,
            (DivisionByZero(_), DivisionByZero(_)) => true,
            (Undefined(_), Undefined(_)) => true,
            (BadSyntax(_, _), BadSyntax(_, _)) => true,
            (SystemError(_), SystemError(_)) => true,
            (ApplyError(_), ApplyError(_)) => true,
            (ArityMismatch(_, _, _), ArityMismatch(_, _, _)) => true,
            (TypeMismatch(_, _), TypeMismatch(_, _)) => true,
            (IndexOutOfRange(_, _, _, _), IndexOutOfRange(_, _, _, _)) => true,
            (_, _) => false,
        }
    }
}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LispError::*;

        match self {
            EndOfInput => write!(f, ""),
            Interrupted => write!(f, "User interrupt"),
            DivisionByZero(sym) => write!(f, "Error in {}: division by zero", sym),
            ReadError(err) => write!(f, "read: {}", err),
            AssignError(err) => write!(f, "set!: {}", err),
            BadSyntax(sym, err) => {
                let msg = if let Some(reason) = err {
                    reason.clone()
                } else {
                    "bad syntax".to_owned()
                };
                write!(f, "{}: {}", sym, msg)
            }
            IndexOutOfRange(sym, index, lower, upper) =>
                write!(f, "{}: index is out of range\n index: {}\n valid range: [{}, {}]", sym, index, lower, upper),
            Undefined(sym) => write!(f, "Error: variable {} is not bound", sym),
            SystemError(err) => write!(f, "Error: {}", err),
            ApplyError(err) => write!(f, "application: {}", err),
            ArityMismatch(sym, expected, given) => write!(
                f,
                "{}: arity mismatch;\n the expected number of arguments does not match the given number\n expected: at least {}\n given: {}",
                sym, expected, given
            ),
            TypeMismatch(expected, given) => write!(f, "type mismatch: expected: {} given: {}", expected, given),
        }
    }
}

impl Error for LispError {
    fn description(&self) -> &str {
        use self::LispError::*;
        match self {
            EndOfInput => "end of input",
            Interrupted => "user interrupt",
            DivisionByZero(_) => "division by zero",
            ReadError(_) => "read error",
            AssignError(_) => "assign error",
            BadSyntax(_, _) => "bad syntax",
            Undefined(_) => "undefined identifier",
            IndexOutOfRange(_, _, _, _) => "index out of range",
            SystemError(_) => "system error",
            ApplyError(_) => "application error",
            ArityMismatch(_, _, _) => "arity mismatch",
            TypeMismatch(_, _) => "type mismatch",
        }
    }
}
