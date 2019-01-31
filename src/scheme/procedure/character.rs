use scheme::types::LispError::*;
use scheme::types::Context;
use scheme::types::LispResult;
use scheme::types::Sexp;
use scheme::types::Sexp::{Char, True, False};

// 标准中只要求对两个操作数进行比较
// 大写字符是有序的
// 小写字符是有序的
// 数码字符是有序的
// 所有的数码字符大于或小于大写字符
// 所有的数码字符大于或小于小写字符
pub fn compare<F>(name: &str, op: F, _context: &mut Context, args: &[Sexp]) -> LispResult<Sexp> where
    F: Fn(char, char) -> bool {
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Char(n) => vals.push(n.clone()),
            _ => return Err(TypeMismatch("char".to_owned(), format!("{}", arg))),
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
