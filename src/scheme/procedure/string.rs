use scheme::types::Context;
use scheme::types::Sexp;
use scheme::types::LispResult;
use scheme::types::LispError::{ArityMismatch, TypeMismatch, IndexOutOfRange};
use scheme::types::Sexp::{Number, Char, Str, Void};

use std::rc::Rc;
use std::cell::RefCell;


// string k 返回新分配的长度为k的字符串，字符串的内容未指定
// string k char 返回新分配的长度为k的字符，所有元素被初始化为char
// 其中k是一个精确非负整数
pub fn make_string(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity == 1 {
        match args[0] {
            Number(k) => Ok(Str(Rc::new(RefCell::new("\u{0}".repeat(k as usize))), true)),
            _ => Err(TypeMismatch("exact nonnegative integer".to_owned(), format!("{}", args[0]))),
        }
    } else if arity == 2 {
        match args[0] {
            Number(k) => match args[1] {
                Char(ch) => Ok(Str(Rc::new(RefCell::new(ch.to_string().repeat(k as usize))), true)),
                _ => Err(TypeMismatch("char".to_owned(), format!("{}", args[1]))),
            }
            _ => Err(TypeMismatch("exact nonnegative integer".to_owned(), format!("{}", args[0]))),
        }
    } else {
        // FIXME expected: 1 to 2
        return Err(ArityMismatch("make-string".to_owned(), 1, arity));
    }
}

pub fn string_length(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 1 {
        return Err(ArityMismatch("string-length".to_owned(), 1, arity));
    }

    match &args[0] {
        Str(s, _) => Ok(Number(s.borrow().chars().count() as i64)),
        _ => Err(TypeMismatch("string".to_owned(), format!("{}", args[0]))),
    }
}

pub fn string_ref(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 2 {
        return Err(ArityMismatch("string_ref".to_owned(), 2, arity));
    }

    match &args[0] {
        Str(s, _) => match args[1] {
            Number(k) => {
                let index = k as usize;
                let ref chars: Vec<char> = s.borrow().chars().collect();
                if k < 0 || index > chars.len() - 1 {
                    Err(IndexOutOfRange("string_ref".to_owned(), index, 0, chars.len() - 1))
                } else {
                    Ok(Char(chars[index]))
                }
            }
            _ => Err(TypeMismatch("exact nonnegative integer".to_owned(), format!("{}", args[1]))),
        }
        _ => Err(TypeMismatch("string".to_owned(), format!("{}", args[0]))),
    }
}

// string_set! str k char
pub fn string_set(_context: &mut Context, args: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = args.len();
    if arity != 3 {
        return Err(ArityMismatch("string_set!".to_owned(), 3, arity));
    }

    match &args[0] {
        Str(s, mutable) => {
            if !mutable {
                return Err(TypeMismatch("string is read only".to_owned(), format!("{}", s.borrow())));
            }
            match args[1] {
                Number(k) => {
                    let index = k as usize;
                    let ref mut chars: Vec<char> = s.borrow().chars().collect();
                    if k < 0 || index > chars.len() - 1 {
                        return Err(IndexOutOfRange("string_set!".to_owned(), index, 0, chars.len() - 1));
                    }
                    match args[2] {
                        Char(ch) => {
                            chars[index] = ch;
                            *s.borrow_mut() = chars.iter().collect();
                            Ok(Void)
                        }
                        _ => Err(TypeMismatch("char".to_owned(), format!("{}", args[2]))),
                    }
                }
                _ => Err(TypeMismatch("exact nonnegative integer".to_owned(), format!("{}", args[1]))),
            }
        }
        _ => Err(TypeMismatch("string".to_owned(), format!("{}", args[0]))),
    }
}
