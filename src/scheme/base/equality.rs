use scheme::types::Sexp;
use scheme::types::LispResult;
use scheme::types::LispError::ArityMismatch;

// eq?
pub fn is_eq(args: &[Sexp]) -> LispResult<Sexp> {
    use self::Sexp::*;
    let arity = args.len();
    if arity != 2 {
        return Err(ArityMismatch("eq?".to_string(), 2, arity));
    }

    let res = match (&args[0], &args[1]) {
        (Nil, Nil) => true,
        (True, True) => true,
        (False, False) => true,
        (Number(n1), Number(n2)) => n1 == n2,   // (= n1 n2) for eqv?
        (Char(c1), Char(c2)) => c1 == c2,   // (char=? c1 c2) for eqv?
        (Symbol(s1), Symbol(s2)) => s1 == s2,   // (string=? s1 s2)
        (Str(str1, true), Str(str2, true)) => str1.as_ptr() == str2.as_ptr(),
        (Str(str1, false), Str(str2, false)) => str1 == str2,
        (List(xs1), List(xs2)) => xs1.as_ptr() == xs2.as_ptr(),
        (Procedure { name: _, func: f1 }, Procedure { name: _, func: f2 }) => f1 == f2,
        (Closure { name: name1, .. }, Closure { name: name2, .. }) => name1 == name2,
        _ => false,
    };
    Ok(Sexp::bool(res))
}

pub fn is_equal(args: &[Sexp]) -> LispResult<Sexp> {
    use self::Sexp::*;
    let arity = args.len();
    if arity != 2 {
        return Err(ArityMismatch("equal?".to_string(), 2, arity));
    }
    let res = match (&args[0], &args[1]) {
        (Str(str1, _), Str(str2, _)) => str1 == str2,
        _ => args[0] == args[1],
    };
    Ok(Sexp::bool(res))
}
