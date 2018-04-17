extern crate regex;

use self::regex::Regex;
use std::error::Error;
use std::io::{self, Stdin, Write};
use std::str;

use scheme::types::LispError;
use scheme::types::LispObject;
use scheme::types::Sexp;

#[derive(Debug)]
enum Token {
    UnquoteSplicing,
    Number(String),
    Str(String),
    Symbol(String),
    Pound(String), // #b #x #t #f
    Char(char),
    Rune(char),
}

pub struct Reader<'a> {
    re_number: Regex,
    re_string: Regex,
    re_char: Regex,
    re_symbol: Regex,
    input: Stdin,
    prompt: &'a str, // 提示符
    scope: i32,      // 嵌套深度
    string: bool,    // 字符串内部
    quote: bool,     // 遇到引号
    line: String,    // 当前行
                     // position: i32,   // 当前位置
}

// FIXME
impl<'a> Iterator for Reader<'a> {
    type Item = LispObject;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.read())
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

impl<'a> Reader<'a> {
    pub fn new(prompt: &'a str) -> Self {
        Reader {
            re_string: Regex::new(r#"^"((\\.|[^"])*)""#).unwrap(),
            re_char: Regex::new(r"#\\([[:alpha:]]+|.)").unwrap(),
            re_number: Regex::new(r"^[-+]?\d+").unwrap(),
            re_symbol: Regex::new(r"^[^.#;'`,\s\(\)][^#;'`,\s\(\)]*").unwrap(),
            prompt: prompt,
            scope: 0,
            // position: 0,
            string: false,
            quote: false,
            input: io::stdin(),
            line: String::new(),
        }
    }

    pub fn read(&mut self) -> LispObject {
        let t = self.next_token()?;
        self.read_token(t)
    }

    fn read_token(&mut self, token: Token) -> LispObject {
        match token {
            Token::Rune('(') => self.parse_list(),
            Token::Rune(')') => {
                if self.scope < 0 {
                    return self.parse_error("unexpected `)'");
                }
                Ok(Sexp::Nil)
            }
            Token::Rune('.') => {
                if self.scope <= 0 {
                    return self.parse_error("illegal use of `.'");
                }
                Ok(Sexp::Dot)
            }
            // 'a => (quote a)
            Token::Rune('\'') => {
                let head = Sexp::Symbol("quote".to_owned());
                let datum = self.read()?;
                let val = vec![head, datum];
                Ok(Sexp::List(val, Box::new(Sexp::Nil)))
            }
            // `a => (quasiquote a)
            Token::Rune('`') => {
                let head = Sexp::Symbol("quasiquote".to_owned());
                let datum = self.read()?;
                let val = vec![head, datum];
                Ok(Sexp::List(val, Box::new(Sexp::Nil)))
            }
            // ,a => (unquote a)
            Token::Rune(',') => {
                let head = Sexp::Symbol("unquote".to_owned());
                let datum = self.read()?;
                let val = vec![head, datum];
                Ok(Sexp::List(val, Box::new(Sexp::Nil)))
            }
            // ,@a => (unquote-splicing a)
            Token::UnquoteSplicing => {
                let head = Sexp::Symbol("unquote-splicing".to_owned());
                let datum = self.read()?;
                let val = vec![head, datum];
                Ok(Sexp::List(val, Box::new(Sexp::Nil)))
            }
            Token::Number(lex) => self.parse_number(lex.as_str()),
            Token::Str(lex) => Ok(Sexp::Str(lex)),
            Token::Char(lex) => Ok(Sexp::Char(lex)),
            Token::Symbol(lex) => Ok(Sexp::Symbol(lex)),
            Token::Pound(lex) => self.parse_pound(lex.as_str()),
            Token::Rune(lex) => self.parse_error(format!("unexpected `{}'", lex).as_str()),
        }
    }

    fn parse_list(&mut self) -> LispObject {
        let mut vec = Vec::new();
        loop {
            let expr = self.read()?;
            if expr == Sexp::Dot {
                let rest = self.read()?;
                if self.read()? != Sexp::Nil {
                    return self.parse_error("illegal use of `.'");
                }
                match rest {
                    Sexp::List(head, tail) => {
                        let mut v = head;
                        vec.append(&mut v);
                        return Ok(Sexp::List(vec, tail));
                    }
                    _ => return Ok(Sexp::List(vec, Box::new(rest))),
                }
            } else if expr == Sexp::Nil {
                break;
            }
            vec.push(expr);
        }
        Ok(Sexp::List(vec, Box::new(Sexp::Nil)))
    }

    // TODO 大整数
    fn parse_number<'b>(&mut self, lex: &'b str) -> LispObject {
        match lex.parse::<i64>() {
            Ok(n) => Ok(Sexp::Number(n)),
            Err(err) => self.parse_error(err.description()),
        }
    }

    // See also https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Additional-Notations.html
    fn parse_pound<'b>(&mut self, lex: &'b str) -> LispObject {
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
        self.quote = false;
        Err(LispError::ParseError(err.to_owned()))
    }

    // 忽略空白和注释
    fn skip_whitespace(&mut self) {
        if !self.string {
            self.line = self.line.trim_left().to_string();
            if self.line.starts_with(';') {
                self.line.clear();
            }
        }
    }

    // 打印提示符并读取输入
    fn read_line(&mut self) {
        self.skip_whitespace();
        while self.line.is_empty() {
            if self.scope > 0 {
                for _ in 0..self.scope {
                    print!("..");
                }
                print!(" ");
            } else if !(self.quote || self.string) {
                print!("{}", self.prompt);
            }
            io::stdout().flush().unwrap();
            self.input.read_line(&mut self.line).unwrap();
            self.skip_whitespace();
        }
    }

    fn next_token(&mut self) -> Result<Token, LispError> {
        self.read_line();

        if self.quote {
            self.quote = false;
        }

        let line = self.line.clone();

        if self.line.chars().peekable().peek() == Some(&'"') {
            for cap in self.re_string.captures_iter(&line) {
                self.line = self.line.replacen(&cap[0], "", 1);
                return Ok(Token::Str(cap[1].to_owned()));
            }
            self.string = true;
            let mut buffer = self.line.clone();
            loop {
                self.line.clear();
                self.read_line();
                buffer.push_str(self.line.as_str());
                for cap in self.re_string.captures_iter(&buffer) {
                    let rest = &buffer[cap[0].len()..];
                    self.line = rest.to_string();
                    self.string = false;
                    return Ok(Token::Str(cap[1].to_owned()));
                }
            }
        }

        for cap in self.re_char.captures_iter(&line) {
            self.line = self.line.replacen(&cap[0], "", 1);
            match name_to_char(&cap[1]) {
                Some(c) => return Ok(Token::Char(c)),
                None => {
                    return self.parse_error(
                        format!("bad character constant: {}", &cap[0]).as_str(),
                    );
                }
            }
        }

        for cap in self.re_number.captures_iter(&line) {
            self.line = self.line.replacen(&cap[0], "", 1);
            return Ok(Token::Number(cap[0].to_owned()));
        }

        for cap in self.re_symbol.captures_iter(&line) {
            self.line = self.line.replacen(&cap[0], "", 1);
            return Ok(Token::Symbol(cap[0].to_lowercase()));
        }

        let first = self.line.remove(0);
        match first {
            '(' => {
                self.scope += 1;
                return Ok(Token::Rune(first));
            }
            ')' => {
                self.scope -= 1;
                return Ok(Token::Rune(first));
            }
            '\'' => {
                self.quote = true;
                self.skip_whitespace(); // quote忽略符号前的空白
                return Ok(Token::Rune(first));
            }
            '`' => {
                self.quote = true;
                self.skip_whitespace();
                return Ok(Token::Rune(first));
            }
            ',' => {
                self.quote = true;
                if self.line.chars().peekable().peek() == Some(&'@') {
                    self.line.remove(0);
                    self.skip_whitespace();
                    return Ok(Token::UnquoteSplicing);
                }
                self.skip_whitespace();
                return Ok(Token::Rune(first));
            }
            '#' => {
                let next = self.line.chars().next();
                if next.is_some() {
                    self.line.remove(0);
                    return Ok(Token::Pound(next.unwrap().to_lowercase().to_string()));
                }
                return self.parse_error(format!("{}", first).as_str());
            }
            _ => return Ok(Token::Rune(first)),
        }
    }
}
