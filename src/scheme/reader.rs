extern crate regex;

use scheme::data::LispResult;
use scheme::data::error::LispError;
use scheme::data::value::Sexp;

use std::error::Error;
use std::io::{self, Stdin, BufRead, Write};
use std::cell::RefCell;
use std::rc::Rc;
use std::str;

use self::regex::Regex;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    EndOfFile,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    Number(String),
    Str(String),
    Symbol(String),
    Sharp(String),
    Char(char),
    Rune(char),
}

pub struct Reader<'a> {
    re_string: Regex,
    re_char: Regex,
    re_symbol: Regex,
    re_sharp: Regex,
    // 缓冲区指针
    tip: usize,
    // 当前行
    line_buffer: String,
    last_paren: char,
    // 嵌套深度
    nest_level: isize,
    // 偷看
    lookahead: Option<Token>,
    // 字符串内部
    string: bool,
    stdin: Stdin,
    input: Option<&'a mut BufRead>,
    ps1: &'a str,
    ps2: &'a str,
}

impl Iterator for Reader<'_> {
    type Item = LispResult<Sexp>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.read() {
            Ok(expr) => Some(Ok(expr)),
            Err(LispError::EndOfInput) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

#[allow(dead_code)]
impl<'a> Reader<'a> {
    pub fn new() -> Self {
        Reader {
            re_string: Regex::new(r#"^"((\\.|[^"])*)""#).unwrap(),
            re_char: Regex::new(r"^#\\(\S[^()\[\]{}\s]*|\s)").unwrap(),
            re_symbol: Regex::new(r#"^[^#;"'`,\s()\[\]{}]+"#).unwrap(),
            re_sharp: Regex::new(r"^#(\S[^()\[\]{}\s]*|\s)?").unwrap(),
            tip: 0,
            line_buffer: String::new(),
            last_paren: '\u{0}',
            nest_level: 0,
            lookahead: None,
            string: false,
            stdin: io::stdin(),
            input: None,
            ps1: "scheme> ",
            ps2: "",
        }
    }

    pub fn set_input(&mut self, input: &'a mut BufRead) {
        self.input = Some(input);
    }

    pub fn set_prompt(&mut self, ps1: &'a str, ps2: &'a str) {
        self.ps1 = ps1;
        self.ps2 = ps2;
    }

    pub fn read(&mut self) -> LispResult<Sexp> {
        use self::Sexp::*;

        self.last_paren = '\u{0}';
        self.nest_level = 0;
        self.string = false;

        let mut list_level = 0;
        let mut dot_count = 0;  // 当前列表中的dot个数
        let mut dot_stack = vec![];
        let mut current_list = vec![]; // 保存当前列表中的表达式
        let mut list_stack = vec![];
        let mut macros = vec![];
        let mut macro_stack = vec![];

        loop {
            let token = self.next_token()?;
            match token {
                Token::EndOfFile => if list_level > 0 {
                    return self.read_error_eof("list");
                } else {
                    return Err(LispError::EndOfInput);
                }
                Token::Quote => macros.push(Symbol("quote".to_owned())),
                Token::Quasiquote => macros.push(Symbol("quasiquote".to_owned())),
                Token::Unquote => macros.push(Symbol("unquote".to_owned())),
                Token::UnquoteSplicing => macros.push(Symbol("unquote-splicing".to_owned())),
                Token::Rune('(') => {
                    if list_level > 0 {
                        dot_stack.push(dot_count);
                        dot_count = 0;
                        list_stack.push(current_list.clone());
                        current_list.clear();
                    }
                    macro_stack.push(macros.clone());
                    macros.clear();
                    list_level += 1;
                }
                Token::Rune(')') => {
                    list_level -= 1;

                    if let Some(m) = macro_stack.pop() {
                        macros = m;
                    }

                    let list = if current_list.is_empty() {
                        Nil
                    } else if dot_count > 0 {
                        if current_list.len() == 1 {
                            // (. x) => x
                            current_list.pop().unwrap()
                        } else if let Some(last) = current_list.pop() {
                            // (a . (b)) = > (a b)
                            if let List(xs) = last {
                                for i in *xs {
                                    current_list.push(i)
                                }
                            } else {
                                current_list.push(last)
                            }
                            List(Box::new(current_list))
                        } else {
                            List(Box::new(current_list))
                        }
                    } else {
                        current_list.push(Nil);
                        List(Box::new(current_list))
                    };

                    if list_stack.is_empty() {
                        return Ok(Reader::expand_macro(&mut macros, &list));
                    }
                    let mut outer = list_stack.pop().unwrap();
                    outer.push(Reader::expand_macro(&mut macros, &list));
                    current_list = outer;
                    dot_count = dot_stack.pop().unwrap();
                }
                Token::Rune('.') => {
                    if list_level == 0 || dot_count > 0 || self.lookahead()? == Token::Rune(')') {
                        return self.read_error("unexpected dot (.)");
                    }
                    dot_count += 1
                }
                _ => {
                    if dot_count > 0 && self.lookahead()? != Token::Rune(')') {
                        return self.read_error("more than one item found after dot (.)");
                    }
                    let expr = self.read_atom(token)?;
                    if list_level == 0 {
                        return Ok(Reader::expand_macro(&mut macros, &expr));
                    }
                    current_list.push(Reader::expand_macro(&mut macros, &expr));
                }
            }
        }
    }

    // Expands reader macros
    fn expand_macro(macros: &mut Vec<Sexp>, inner: &Sexp) -> Sexp {
        use self::Sexp::*;
        let mut expr = inner.clone();
        while let Some(m) = macros.pop() {
            expr = List(Box::new(vec![m, expr, Nil]));
        }
        expr
    }

    fn read_atom(&mut self, token: Token) -> LispResult<Sexp> {
        use self::Sexp::*;
        match token {
            Token::Number(lex) => self.parse_number(&lex),
            Token::Str(lex) => Ok(Str(Rc::new(RefCell::new(lex)), false)),
            Token::Char(lex) => Ok(Char(lex)),
            Token::Symbol(lex) => Ok(Symbol(lex)),
            Token::Sharp(lex) => self.parse_sharp_sign(&lex),
            _ => self.read_error(&format!("unexpected '{:?}'", token)),
        }
    }

    // 忽略空白和注释
    fn skip_whitespace(&mut self) {
        let mut ws = String::new();
        for c in self.line_buffer[self.tip..].chars() {
            if c.is_whitespace() {
                ws.push(c);
            } else if c == ';' {
                self.tip = self.line_buffer.len();
                return;
            } else {
                break;
            }
        }
        self.tip += ws.len();
    }

    fn read_line(&mut self, continue_read: bool) -> LispResult<()> {
        self.skip_whitespace();
        while self.tip >= self.line_buffer.len() {
            self.line_buffer.clear();
            self.tip = 0;
            let ret = if let Some(ref mut input) = self.input {
                input.read_line(&mut self.line_buffer)
            } else {
                let prompt = if self.nest_level == 0 && !continue_read && !self.string {
                    &self.ps1
                } else {
                    &self.ps2
                };
                print!("{}", prompt);
                io::stdout().flush().unwrap();
                // Note read_line will add a newline to the end anyway
                self.stdin.lock().read_line(&mut self.line_buffer)
            };

            if let Err(err) = ret {
                return Err(LispError::ReadError(err.to_string()));
            }
            if self.line_buffer.is_empty() {
                return Err(LispError::EndOfInput);
            }
            if !self.string {
                self.skip_whitespace();
            }
        }
        Ok(())
    }

    fn lookahead(&mut self) -> LispResult<Token> {
        if self.lookahead.is_some() {
            let token = self.lookahead.clone();
            Ok(token.unwrap())
        } else {
            let token = self.next_token()?;
            self.lookahead = Some(token.clone());
            Ok(token)
        }
    }

    fn next_token(&mut self) -> LispResult<Token> {
        use self::Token::*;

        if self.lookahead.is_some() {
            let token = self.lookahead.clone();
            self.lookahead = None;
            return Ok(token.unwrap());
        }

        if let Err(err) = self.read_line(false) {
            match err {
                LispError::EndOfInput => return Ok(EndOfFile),
                _ => return Err(err)
            }
        }

        // 解析字符串
        // TODO 处理字符中的转义和字符序列
        if self.line_buffer[self.tip..].starts_with("\"") {
            for cap in self.re_string.captures_iter(&self.line_buffer[self.tip..]) {
                self.tip += cap[0].len();
                return Ok(Str(cap[1].to_owned()));
            }
            self.string = true;
            let mut buffer = self.line_buffer[self.tip..].to_string();
            loop {
                self.tip = self.line_buffer.len();
                if let Err(err) = self.read_line(false) {
                    match err {
                        LispError::EndOfInput => return self.read_error_eof("string"),
                        _ => return Err(err)
                    }
                }
                buffer.push_str(&self.line_buffer);
                for cap in self.re_string.captures_iter(&buffer) {
                    self.tip += self.line_buffer.len() - (buffer.len() - cap[0].len());
                    self.string = false;
                    return Ok(Str(cap[1].to_owned()));
                }
            }
        }

        // 解析数字常量
        let mut chars = self.line_buffer[self.tip..].chars();
        let first = chars.next().unwrap();
        if first.is_ascii_digit() || "+-".contains(first) {
            let mut buffer = String::new();
            buffer.push(first);
            for c in chars {
                if c.is_ascii_digit() {
                    buffer.push(c);
                } else if Reader::is_delimiter(c) {
                    if buffer == "+" || buffer == "-" {
                        break;
                    }
                    self.tip += buffer.len();
                    return Ok(Number(buffer));
                } else {
                    break;
                }
            }
        }

        for cap in self.re_symbol.captures_iter(&self.line_buffer[self.tip..]) {
            self.tip += cap[0].len();
            return if &cap[0] == "." {
                Ok(Rune('.'))
            } else {
                Ok(Symbol(cap[0].to_lowercase()))
            };
        }

        if self.line_buffer[self.tip..].starts_with(",@") {
            self.tip += 2;
            if let Err(err) = self.read_line(true) {
                match err {
                    LispError::EndOfInput => return self.read_error_eof("unquote (,@)"),
                    _ => return Err(err)
                }
            }
            return Ok(UnquoteSplicing);
        }

        let first = self.line_buffer[self.tip..].chars().next().unwrap();
        match first {
            '(' => {
                self.tip += 1;
                self.last_paren = '(';
                self.nest_level += 1;
                return Ok(Rune(first));
            }
            ')' => {
                self.tip += 1;
                if self.nest_level == 0 || self.last_paren == '[' {
                    return self.read_error("unexpected ')'");
                }
                self.last_paren = ')';
                self.nest_level -= 1;
                return Ok(Rune(first));
            }
            '[' => {
                self.tip += 1;
                self.last_paren = '[';
                self.nest_level += 1;
                return Ok(Rune('('));
            }
            ']' => {
                self.tip += 1;
                if self.nest_level == 0 || self.last_paren == '(' {
                    return self.read_error("unexpected ']'");
                }
                self.last_paren = ']';
                self.nest_level -= 1;
                return Ok(Rune(')'));
            }
            '\'' => {
                self.tip += 1;
                if let Err(err) = self.read_line(true) {
                    match err {
                        LispError::EndOfInput => return self.read_error_eof("quote (')"),
                        _ => return Err(err)
                    }
                }
                return Ok(Quote);
            }
            '`' => {
                self.tip += 1;
                if let Err(err) = self.read_line(true) {
                    match err {
                        LispError::EndOfInput => return self.read_error_eof("quasiquote (`)"),
                        _ => return Err(err)
                    }
                }
                return Ok(Quasiquote);
            }
            ',' => {
                self.tip += 1;
                if let Err(err) = self.read_line(true) {
                    match err {
                        LispError::EndOfInput => return self.read_error_eof("unquote (,)"),
                        _ => return Err(err)
                    }
                }
                return Ok(Unquote);
            }
            '#' => {
                // 解析字符
                for cap in self.re_char.captures_iter(&self.line_buffer[self.tip..]) {
                    self.tip += cap[0].len();
                    match name_to_char(&cap[1]) {
                        Some(c) => return Ok(Char(c)),
                        None => return self.read_error(&format!("unknown character name: {}", &cap[1])),
                    }
                }
                // sharp-sign
                let caps = self.re_sharp.captures(&self.line_buffer[self.tip..]).unwrap();
                self.tip += caps[0].len();
                return Ok(Sharp(caps[1].to_lowercase()));
            }
            _ => {
                self.tip += first.to_string().len();
                return self.read_error(&format!("unexpected character '{}'", first));
            }
        }
    }

    // TODO 大整数
    fn parse_number(&mut self, lex: &str) -> LispResult<Sexp> {
        match lex.parse::<i64>() {
            Ok(n) => Ok(Sexp::Number(n)),
            Err(err) => self.read_error(err.description()),
        }
    }

    // See also https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Additional-Notations.html
    fn parse_sharp_sign(&mut self, lex: &str) -> LispResult<Sexp> {
        use self::Sexp::*;
        match lex {
            "t" => Ok(True),
            "f" => Ok(False),
            _ => self.read_error(&format!("invalid sharp-sign prefix: {:?}", lex)),
        }
    }

    fn is_delimiter(c: char) -> bool {
        c.is_whitespace() || "()[]{}|\";".contains(c)
    }

    fn read_error<T>(&mut self, err: &str) -> LispResult<T> {
        self.last_paren = '\u{0}';
        self.nest_level = 0;
        self.string = false;
        Err(LispError::ReadError(err.to_owned()))
    }

    fn read_error_eof<T>(&mut self, token: &str) -> LispResult<T> {
        self.last_paren = '\u{0}';
        self.nest_level = 0;
        self.string = false;
        Err(LispError::ReadError(format!("unexpected end-of-file reading {}", token)))
    }
}

// 把字符名称转换成ASCII
// TODO 补充完整ASCII中所有的不可打印字符
// #\newline应该根据平台决定是#\linefeed还是#\return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
pub fn name_to_char(name: &str) -> Option<char> {
    match name.to_lowercase().as_str() {
        "altmode" => Some('\x1b'), // ESC
        "backnext" => Some('\x1f'), // US
        "backspace" => Some('\x08'), // BS
        "call" => Some('\x1a'), // SUB
        "linefeed" => Some('\x0a'), // LF
        "page" => Some('\x0c'), // FF
        "return" => Some('\x0d'), // CR
        "rubout" => Some('\x7f'), // DEL
        "space" => Some('\x20'),
        "tab" => Some('\x09'), // HT
        "newline" => Some('\n'),
        _ => if name.chars().count() == 1 {
            // 单个unicode字符
            name.chars().next()
        } else {
            None
        }
    }
}

