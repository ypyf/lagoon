extern crate regex;
extern crate rustyline;

use scheme::types::LispError;
use scheme::types::LispResult;
use scheme::types::Sexp;

use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::str;

use self::regex::Regex;
use self::rustyline::error::ReadlineError;
use self::rustyline::Editor;
use self::rustyline::config::Configurer;

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

impl fmt::Display for Token {
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
            Char(s) => write!(f, r#"#\{}"#, s),
            Pound(s) => write!(f, "#{}", s),
            Rune(s) => write!(f, "{}", s),
        }
    }
}

pub struct Reader {
    re_number: Regex,
    re_string: Regex,
    re_char: Regex,
    re_symbol: Regex,
    readline: Editor<()>,
    ps1: String,
    ps2: String,
    // 嵌套深度
    scope: isize,
    // 字符串内部
    string: bool,
    // 当前行
    line: String,
}

impl Iterator for Reader {
    type Item = LispResult;
    fn next(&mut self) -> Option<Self::Item> {
        match self.read() {
            Ok(expr) => Some(Ok(expr)),
            Err(_) => None,
        }
    }
}

#[allow(dead_code)]
impl Reader {
    pub fn new() -> Self {
        let mut rl = Editor::<()>::new();
        rl.set_auto_add_history(true);
        rl.set_history_ignore_dups(true);
        rl.set_history_ignore_space(true);
        Reader {
            re_string: Regex::new(r#"^"((\\.|[^"])*)""#).unwrap(),
            re_char: Regex::new(r"^#\\([[:alpha:]]+|.)").unwrap(), // 不包含\n
            re_number: Regex::new(r"^[-+]?\d+").unwrap(),
            re_symbol: Regex::new(r"^[^.#;'`,\s()][^#;'`,\s()]*").unwrap(),
            ps1: "r5rs> ".to_owned(),
            ps2: "... ".to_owned(),
            scope: 0,
            string: false,
            readline: rl,
            line: String::new(),
        }
    }

    pub fn set_prompt(&mut self, ps1: &str, ps2: &str) {
        self.ps1 = ps1.to_owned();
        self.ps2 = ps2.to_owned();
    }

    pub fn read(&mut self) -> LispResult {
        self.scope = 0;
        self.string = false;
        let mut macros = vec![];
        loop {
            match self.next_token() {
                Ok(token) => {
                    match token {
                        Token::Quote => macros.push("quote"),
                        Token::Quasiquote => macros.push("quasiquote"),
                        Token::Unquote => macros.push("unquote"),
                        Token::UnquoteSplicing => macros.push("unquote-splicing"),
                        Token::Rune('(') => {
                            let mut expr = self.read_list()?;
                            while let Some(m) = macros.pop() {
                                let init = vec![Rc::new(Sexp::Symbol(m.to_owned())), expr];
                                expr = Rc::new(Sexp::List(init, Rc::new(Sexp::Nil)));
                            }
                            return Ok(expr);
                        }
                        _ => {
                            let mut expr = self.read_atom(token)?;
                            while let Some(m) = macros.pop() {
                                let init = vec![Rc::new(Sexp::Symbol(m.to_owned())), expr];
                                expr = Rc::new(Sexp::List(init, Rc::new(Sexp::Nil)));
                            }
                            return Ok(expr);
                        }
                    }
                }
                Err(err) => return Err(err)
            }
        }
    }

    fn read_atom(&mut self, token: Token) -> LispResult {
        match token {
            Token::Number(lex) => self.parse_number(lex.as_str()),
            Token::Str(lex) => Ok(Rc::new(Sexp::Str(lex))),
            Token::Char(lex) => Ok(Rc::new(Sexp::Char(lex))),
            Token::Symbol(lex) => Ok(Rc::new(Sexp::Symbol(lex))),
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
        let mut list: Vec<Rc<Sexp>> = vec![]; // 当前列表
        loop {
            match self.next_token() {
                Ok(token) => {
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
                            let mut expr = Rc::new(Sexp::List(list, Rc::new(Sexp::Nil)));
                            while let Some(m) = macros.pop() {
                                let init = vec![Rc::new(Sexp::Symbol(m.to_owned())), expr];
                                expr = Rc::new(Sexp::List(init, Rc::new(Sexp::Nil)));
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

                            let last = self.read_atom(token)?;
                            let mut expr: Sexp;
                            if macros.is_empty() {
                                expr = Sexp::List(list, last);
                            } else {
                                while let Some(m) = macros.pop() {
                                    list.push(Rc::new(Sexp::Symbol(m.to_owned())));
                                }
                                list.push(last);
                                expr = Sexp::List(list, Rc::new(Sexp::Nil));
                                ;
                            }

                            if let Some(mut outer) = stack.pop() {
                                outer.push(Rc::new(expr));
                                list = outer;
                            } else {
                                return Ok(Rc::new(expr));
                            }
                        } else {
                            let mut expr = self.read_atom(token)?;
                            while let Some(m) = macros.pop() {
                                let init = vec![Rc::new(Sexp::Symbol(m.to_owned())), expr];
                                expr = Rc::new(Sexp::List(init, Rc::new(Sexp::Nil)));
                            }
                            list.push(expr)
                        },
                    }
                }
                Err(err) => return Err(err)
            }
        }
    }

    // 忽略空白和注释
    fn skip_whitespace(line: &String) -> String {
        let x = line.trim_start();
        if x.starts_with(';') {
            return "".to_owned();
        }
        x.to_owned()
    }

    // 打印提示符并读取输入
    fn read_line(&mut self, continue_read: bool) -> Result<String, LispError> {
        let mut line = Reader::skip_whitespace(&self.line);
        while line.is_empty() {
            let prompt = if self.scope == 0 && !continue_read && !self.string {
                &self.ps1
            } else {
                &self.ps2
            };
            match self.readline.readline(prompt) {
                Ok(input) => {
                    line = input;
                    if !self.string {
                        line = Reader::skip_whitespace(&line);
                    } else {
                        line.push('\n');
                    }
                }
                Err(ReadlineError::Interrupted) => return Err(LispError::Interrupted),
                Err(ReadlineError::Eof) => return Err(LispError::EndOfInput),
                // FIXME use a distinct error type
                Err(err) => panic!(err),
            };
        }
        Ok(line.to_owned())
    }

    fn next_token(&mut self) -> Result<Token, LispError> {
        use self::Token::*;

        match self.read_line(false) {
            Ok(line) => self.line = line.to_owned(),
            Err(err) => return Err(err)
        }

        let line = self.line.clone();

        // 解析字符串
        // TODO 处理字符中的转义和字符序列
        if let Some('"') = line.chars().peekable().peek() {
            for cap in self.re_string.captures_iter(&line) {
                self.line = self.line.replacen(&cap[0], "", 1);
                return Ok(Str(cap[1].to_owned()));
            }
            self.string = true;
            let mut buffer = self.line.clone();
            buffer.push('\n');
            loop {
                self.line.clear();
                match self.read_line(false) {
                    Ok(line) => self.line = line.to_owned(),
                    Err(err) => return Err(err)
                }
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
            match self.read_line(true) {
                Ok(line) => self.line = line.to_owned(),
                Err(err) => return Err(err)
            }
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
            '\'' => {
                match self.read_line(true) {
                    Ok(line) => self.line = line.to_owned(),
                    Err(err) => return Err(err)
                }
                return Ok(Quote);
            }
            '`' => {
                match self.read_line(true) {
                    Ok(line) => self.line = line.to_owned(),
                    Err(err) => return Err(err)
                }
                return Ok(Quasiquote);
            }
            ',' => {
                match self.read_line(true) {
                    Ok(line) => self.line = line.to_owned(),
                    Err(err) => return Err(err)
                }
                return Ok(Unquote);
            }
            '#' => {
                let next = self.line.chars().next();
                if next.is_some() {
                    self.line.remove(0);
                    return Ok(Pound(next.unwrap().to_lowercase().to_string()));
                }
                return self.parse_error(format!("`{}'", first).as_str());
            }
            _ => return Ok(Rune(first)),
        }
    }

    // TODO 大整数
    fn parse_number(&mut self, lex: &str) -> LispResult {
        match lex.parse::<i64>() {
            Ok(n) => Ok(Rc::new(Sexp::Number(n))),
            Err(err) => self.parse_error(err.description()),
        }
    }

    // See also https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Additional-Notations.html
    fn parse_pound(&mut self, lex: &str) -> LispResult {
        match lex {
            "t" => Ok(Rc::new(Sexp::True)),
            "f" => Ok(Rc::new(Sexp::False)),
            // "i" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 非精确数前缀
            // "e" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 精确数前缀
            // "d" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 10进制数码前缀
            // "x" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 16进制数码前缀
            // "o" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 8进制数码前缀
            // "b" => Ok(Sexp::Symbol(lex.to_owned())), // TODO 2进制数码前缀
            _ => self.parse_error(format!("`#{}'", lex).as_str()),
        }
    }

    fn parse_error<T>(&mut self, err: &str) -> Result<T, LispError> {
        self.scope = 0;
        self.string = false;
        Err(LispError::BadSyntax("read".to_owned(), err.to_owned()))
    }
}

// TODO 补充完整ASCII中所有的不可打印字符
// FIXME newline应该根据平台决定是linefeed还是return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
pub fn name_to_char(name: &str) -> Option<char> {
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
#[allow(dead_code)]
pub fn char_to_name(ch: char) -> String {
    match ch {
        '\x08' => "backspace".to_owned(),
        '\n' => "newline".to_owned(),
        '\r' => "return".to_owned(),
        ' ' => "space".to_owned(),
        _ => ch.to_string(),
    }
}
