extern crate regex;

use self::regex::Regex;
use std::error::Error;
use std::fmt;
use std::io::{self, Stdin, Write};
use std::str;

use scheme::types::LispError;
use scheme::types::LispResult;
use scheme::types::Sexp;

#[derive(Debug, PartialEq)]
enum Token {
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    Number(String),
    Str(String),
    Symbol(String),
    Pound(String),
    Char(char),
    Rune(char),
}

impl<'a> fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;

        match self {
            Quote => write!(f, "'"),
            Quasiquote => write!(f, "`"),
            Unquote => write!(f, ","),
            UnquoteSplicing => write!(f, ",@"),
            Number(s) => write!(f, "{}", s),
            Str(s) => write!(f, "{:?}", s),
            Symbol(s) => write!(f, "{}", s),
            Char(s) => write!(f, "#\\{}", s),
            Pound(s) => write!(f, "#{}", s),
            Rune(s) => write!(f, "{}", s),
        }
    }
}

pub struct Reader<'a> {
    re_number: Regex,
    re_string: Regex,
    re_char: Regex,
    re_symbol: Regex,
    input: Stdin,
    prompt: &'a str, // 提示符
    scope: isize,    // 嵌套深度
    string: bool,    // 字符串内部
    line: String,    // 当前行
}

// FIXME
impl<'a> Iterator for Reader<'a> {
    type Item = LispResult;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.read())
    }
}

impl<'a> Reader<'a> {
    pub fn new(prompt: &'a str) -> Self {
        Reader {
            re_string: Regex::new(r#"^"((\\.|[^"])*)""#).unwrap(),
            re_char: Regex::new(r"^#\\([[:alpha:]]+|.)").unwrap(), // 不包含\n
            re_number: Regex::new(r"^[-+]?\d+").unwrap(),
            re_symbol: Regex::new(r"^[^.#;'`,\s\(\)][^#;'`,\s\(\)]*").unwrap(),
            prompt: prompt,
            scope: 0,
            string: false,
            input: io::stdin(),
            line: String::new(),
        }
    }

    pub fn read(&mut self) -> LispResult {
        let mut macros = vec![];
        while let Ok(token) = self.next_token() {
            match token {
                Token::Quote => macros.push("quote"),
                Token::Quasiquote => macros.push("quasiquote"),
                Token::Unquote => macros.push("unquote"),
                Token::UnquoteSplicing => macros.push("unquote-splicing"),
                Token::Rune('(') => {
                    let mut expr = self.read_list()?;
                    while let Some(m) = macros.pop() {
                        let init = vec![Sexp::Symbol(m.to_owned()), expr].into_boxed_slice();
                        expr = Sexp::List(init, Box::new(Sexp::Nil));
                    }
                    return Ok(expr);
                }
                _ => {
                    let mut expr = self.read_atom(token)?;
                    while let Some(m) = macros.pop() {
                        let init = vec![Sexp::Symbol(m.to_owned()), expr].into_boxed_slice();
                        expr = Sexp::List(init, Box::new(Sexp::Nil));
                    }
                    return Ok(expr);
                }
            }
        }
        self.parse_error("unexpected EOI")
    }

    fn read_atom(&mut self, token: Token) -> LispResult {
        match token {
            Token::Number(lex) => self.parse_number(lex.as_str()),
            Token::Str(lex) => Ok(Sexp::Str(lex)),
            Token::Char(lex) => Ok(Sexp::Char(lex)),
            Token::Symbol(lex) => Ok(Sexp::Symbol(lex)),
            Token::Pound(lex) => self.parse_pound(lex.as_str()),
            Token::Rune('.') => self.parse_error("illegal use of `.'"),
            _ => self.parse_error(format!("unexpected `{}'", token).as_str()),
        }
    }

    fn read_list(&mut self) -> LispResult {
        use self::Token::*;
        let mut dot = false;
        let mut macros = vec![];
        let mut stack = vec![]; // 保存未处理完的外层列表
        let mut list: Vec<Sexp> = vec![]; // 当前列表
        while let Ok(token) = self.next_token() {
            match token {
                Quote => macros.push("quote"),
                Quasiquote => macros.push("quasiquote"),
                Unquote => macros.push("unquote"),
                UnquoteSplicing => macros.push("unquote-splicing"),
                Rune('(') => {
                    stack.push(list.clone());
                    list.clear();
                }
                Rune(')') => {
                    let mut expr = Sexp::List(list.into_boxed_slice(), Box::new(Sexp::Nil));
                    while let Some(m) = macros.pop() {
                        let init = vec![Sexp::Symbol(m.to_owned()), expr].into_boxed_slice();
                        expr = Sexp::List(init, Box::new(Sexp::Nil));
                    }
                    if let Some(mut outer) = stack.pop() {
                        outer.push(expr);
                        list = outer;
                    } else {
                        return Ok(expr);
                    }
                }
                Rune('.') => {
                    if list.is_empty() {
                        return self.parse_error("illegal use of `.'");
                    }
                    dot = true;
                }
                _ => if dot {
                    dot = false;
                    if self.next_token()? != Rune(')') {
                        return self.parse_error("illegal use of `.'");
                    }

                    let mut last = self.read_atom(token)?;
                    let mut expr: Sexp;
                    if macros.is_empty() {
                        expr = Sexp::List(list.into_boxed_slice(), Box::new(last));
                    } else {
                        while let Some(m) = macros.pop() {
                            list.push(Sexp::Symbol(m.to_owned()));
                        }
                        list.push(last);
                        expr = Sexp::List(list.into_boxed_slice(), Box::new(Sexp::Nil));;
                    }

                    if let Some(mut outer) = stack.pop() {
                        outer.push(expr);
                        list = outer;
                    } else {
                        return Ok(expr);
                    }
                } else {
                    let mut expr = self.read_atom(token)?;
                    while let Some(m) = macros.pop() {
                        let init = vec![Sexp::Symbol(m.to_owned()), expr].into_boxed_slice();
                        expr = Sexp::List(init, Box::new(Sexp::Nil));
                    }
                    list.push(expr)
                },
            }
        }
        self.parse_error("expected a `)' to close `('")
    }

    // 忽略空白和注释
    fn skip_whitespace(&mut self) {
        self.line = self.line.trim_left().to_string();
        if self.line.starts_with(';') {
            self.line.clear();
        }
    }

    // 打印提示符并读取输入
    fn read_line(&mut self, prompt: bool) {
        self.skip_whitespace();
        while self.line.is_empty() {
            if self.scope == 0 && prompt {
                print!("{}", self.prompt);
            }
            io::stdout().flush().unwrap();
            self.input.read_line(&mut self.line).unwrap();
            if !self.string {
                self.skip_whitespace();
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, LispError> {
        use self::Token::*;

        self.read_line(true);

        let line = self.line.clone();

        // 解析字符串
        if let Some('"') = line.chars().peekable().peek() {
            for cap in self.re_string.captures_iter(&line) {
                self.line = self.line.replacen(&cap[0], "", 1);
                return Ok(Str(cap[1].to_owned()));
            }
            self.string = true;
            let mut buffer = self.line.clone();
            loop {
                self.line.clear();
                self.read_line(false);
                buffer.push_str(self.line.as_str());
                for cap in self.re_string.captures_iter(&buffer) {
                    let rest = &buffer[cap[0].len()..];
                    self.line = rest.to_string();
                    self.string = false;
                    return Ok(Str(cap[1].to_owned()));
                }
            }
        }

        // 解析字符
        for cap in self.re_char.captures_iter(&line) {
            self.line = self.line.replacen(&cap[0], "", 1);
            match name_to_char(&cap[1]) {
                Some(c) => return Ok(Char(c)),
                None => {
                    return self.parse_error(
                        format!("bad character constant: {}", &cap[0]).as_str(),
                    );
                }
            }
        }

        for cap in self.re_number.captures_iter(&line) {
            self.line = self.line.replacen(&cap[0], "", 1);
            return Ok(Number(cap[0].to_owned()));
        }

        for cap in self.re_symbol.captures_iter(&line) {
            self.line = self.line.replacen(&cap[0], "", 1);
            return Ok(Symbol(cap[0].to_lowercase()));
        }

        if line.starts_with(",@") {
            self.line = self.line.replacen(",@", "", 1);
            return Ok(UnquoteSplicing);
        }

        let first = self.line.remove(0);
        match first {
            '(' => {
                self.scope += 1;
                return Ok(Rune(first));
            }
            ')' => {
                self.scope -= 1;
                return Ok(Rune(first));
            }
            '\'' => return Ok(Quote),
            '`' => return Ok(Quasiquote),
            ',' => return Ok(Unquote),
            '#' => {
                let next = self.line.chars().next();
                if next.is_some() {
                    self.line.remove(0);
                    return Ok(Pound(next.unwrap().to_lowercase().to_string()));
                }
                return self.parse_error(format!("{}", first).as_str());
            }
            _ => return Ok(Rune(first)),
        }
    }

    // TODO 大整数
    fn parse_number<'b>(&mut self, lex: &'b str) -> LispResult {
        match lex.parse::<i64>() {
            Ok(n) => Ok(Sexp::Number(n)),
            Err(err) => self.parse_error(err.description()),
        }
    }

    // See also https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Additional-Notations.html
    fn parse_pound<'b>(&mut self, lex: &'b str) -> LispResult {
        match lex {
            "t" => Ok(Sexp::True),
            "f" => Ok(Sexp::False),
            "i" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 非精确数前缀
            "e" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 精确数前缀
            "d" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 10进制数码前缀
            "x" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 16进制数码前缀
            "o" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 8进制数码前缀
            "b" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 2进制数码前缀
            _ => self.parse_error(format!("bad syntax `#{}'", lex).as_str()),
        }
    }

    fn parse_error<'b, T>(&mut self, err: &'b str) -> Result<T, LispError> {
        self.scope = 0;
        self.string = false;
        Err(LispError::ParseError(err.to_owned()))
    }
}

// TODO 补充完整ASCII中所有的不可打印字符
// FIXME newline应该根据平台决定是linefeed还是return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
pub fn name_to_char<'a>(name: &'a str) -> Option<char> {
    if name.len() > 1 {
        // 字符名不区分大小写
        match name.to_lowercase().as_str() {
            "backspace" => Some('\x08'),
            "space" => Some(' '),
            "newline" => Some('\n'),
            "return" => Some('\r'),
            _ => None,
        }
    } else {
        name.chars().next()
    }
}

// TODO 参看 name_to_char
pub fn char_to_name(ch: char) -> String {
    match ch {
        '\x08' => "backspace".to_owned(),
        '\n' => "newline".to_owned(),
        '\r' => "return".to_owned(),
        ' ' => "space".to_owned(),
        _ => ch.to_string(),
    }
}
