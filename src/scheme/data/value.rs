use super::iterator::{ListIterator, ListIntoIterator};
use super::LispResult;
use scheme::context::Context;

use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

pub type HostFunction1 = fn(&[Sexp]) -> LispResult<Sexp>;
pub type HostFunction2 = fn(&mut Context, &[Sexp]) -> LispResult<Sexp>;

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxRule {
    pub pattern: Sexp,
    pub template: Sexp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Transformer {
    pub id: Option<String>,
    pub literals: Vec<String>,
    pub rules: Vec<SyntaxRule>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Void,
    Nil,
    Char(char),
    Str(Rc<RefCell<String>>, bool),
    True,
    False,
    Number(i64),
    Symbol(String),
    List(Box<Vec<Sexp>>),
    Keyword {
        name: String,
        func: HostFunction2,
    },
    Function {
        name: String,
        func: HostFunction1,
    },
    Procedure {
        name: String,
        func: HostFunction2,
    },
    // Vector(Vec<&'a Sexp<'a>>),
    Closure {
        name: String,
        params: Vec<String>,
        vararg: Option<String>,
        body: Rc<Vec<Sexp>>,
        env: Context,
    },
    Syntax {
        keyword: String,
        transformer: Transformer,
    },
}

impl IntoIterator for Sexp {
    type Item = Sexp;
    type IntoIter = ListIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIntoIterator::new(self)
    }
}

impl<'a> IntoIterator for &'a Sexp {
    type Item = &'a Sexp;
    type IntoIter = ListIterator<'a>;

    // 注意这里的self是引用
    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self)
    }
}

impl Sexp {
    pub fn is_true(&self) -> bool {
        // Scheme中只有#f是假值
        if let Sexp::False = self {
            false
        } else {
            true
        }
    }

    pub fn bool(b: bool) -> Sexp {
        if b {
            Sexp::True
        } else {
            Sexp::False
        }
    }

    // null?
    pub fn is_null(&self) -> bool {
        if let Sexp::Nil = self {
            true
        } else {
            false
        }
    }

    // pair?
    pub fn is_pair(&self) -> bool {
        if let Sexp::List(_) = self {
            true
        } else {
            false
        }
    }

    // list?
    pub fn is_list(&self) -> bool {
        use self::Sexp::*;
        match self {
            Nil => true,
            List(xs) => {
                *xs.last().unwrap() == Nil
            }
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        use self::Sexp::*;
        if let Str(_, _) = self {
            true
        } else {
            false
        }
    }

    // 计算元素个数（不包括尾部的Nil, 不递归计算）
    pub fn list_count(&self) -> usize {
        use self::Sexp::*;
        if let List(xs) = self {
            if self.is_list() {
                xs.len() - 1
            } else {
                xs.len()
            }
        } else {
            panic!()
        }
    }

    pub fn init(&self) -> Option<&[Sexp]> {
        if let Sexp::List(xs) = self {
            let (_, init) = xs.split_last().unwrap();
            Some(init)
        } else {
            None
        }
    }

    pub fn tail(&self) -> Option<&[Sexp]> {
        if let Sexp::List(xs) = self {
            let (_, tail) = xs.split_first().unwrap();
            Some(tail)
        } else {
            None
        }
    }

    // 返回指定下标后的元素，不包括尾部的Nil
    pub fn rest(&self, from: usize) -> Option<&[Sexp]> {
        if let Sexp::List(xs) = self {
            if self.is_list() {
                Some(&xs[from..xs.len() - 1])
            } else {
                Some(&xs[from..])
            }
        } else {
            None
        }
    }

    // 返回列表中的第n个元素，不包含尾部的Nil
    pub fn nth(&self, n: usize) -> Option<&Sexp> {
        if let Sexp::List(xs) = self {
            if n >= self.list_count() {
                return None;
            }
            Some(&xs[n])
        } else {
            None
        }
    }

    // 返回列表，不包含尾部的Nil
    pub fn as_slice(&self) -> Option<&[Self]> {
        if let Sexp::List(xs) = self {
            if self.is_list() {
                let (_, init) = xs.split_last().unwrap();
                Some(init)
            } else {
                Some(xs)
            }
        } else {
            None
        }
    }

    pub fn set_nth(&mut self, n: usize, sexp: &Sexp) {
        if n >= self.list_count() {
            return;
        }
        if let Sexp::List(xs) = self {
            xs[n] = sexp.clone();
        }
    }

    pub fn is_symbol(&self) -> bool {
        if let Sexp::Symbol(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_mutable_string(&self) -> bool {
        if let Sexp::Str(_, true) = self {
            true
        } else {
            false
        }
    }

    pub fn is_immutable_string(&self) -> bool {
        if let Sexp::Str(_, false) = self {
            true
        } else {
            false
        }
    }
}

// 实现 "{}"
impl<'a> fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Sexp::*;

        match self {
            Void => write!(f, "#<void>"),
            Nil => write!(f, "()"),
            True => write!(f, "#t"),
            False => write!(f, "#f"),
            Number(n) => write!(f, "{}", n),
            Symbol(n) => write!(f, "{}", n),
            Char(n) => write!(f, "#\\{}", char_to_name(*n)),
            Str(n, _) => write!(f, "{:?}", n.borrow()), // 字符串输出时显示双引号
            Keyword { name, .. } => write!(f, "#<keyword {}>", name),
            Procedure { name, .. } => write!(f, "#<procedure {}>", name),
            Function { name, .. } => write!(f, "#<procedure {}>", name),
            Closure { name, .. } => if name.is_empty() {
                write!(f, "#<procedure>")
            } else {
                write!(f, "#<procedure {}>", name)
            }
            Syntax { keyword, .. } => write!(f, "#<syntax {}>", keyword),
            List(xs) => {
                let mut string = String::new();
                string.push('(');
                let (last, init) = xs.split_last().unwrap();
                for expr in init {
                    string.push_str(&format!("{} ", expr))
                }
                if *last == Nil {
                    // remove the trailing space
                    string.pop();
                } else {
                    string.push_str(&format!(". {}", last));
                }
                string.push(')');
                write!(f, "{}", string)
            }
        }
    }
}


// 把字符转换成对应的名称
pub fn char_to_name(ch: char) -> String {
    match ch {
        '\x1b' => "altmode".to_string(),
        '\x1f' => "backnext".to_string(),
        '\x08' => "backspace".to_string(),
        '\x1a' => "call".to_string(),
        '\x0c' => "page".to_string(),
        '\x0d' => "return".to_string(),
        '\x7f' => "rubout".to_string(),
        '\x20' => "space".to_string(),
        '\x09' => "tab".to_string(),
        '\n' => "newline".to_string(),
        _ => ch.to_string(),
    }
}
