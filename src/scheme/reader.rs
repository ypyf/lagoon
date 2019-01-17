extern crate regex;
extern crate rustyline;
extern crate dirs;

use scheme::types::LispError;
use scheme::types::LispResult;
use scheme::types::Sexp;

use std::error::Error;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;
use std::str;

use self::regex::Regex;
use self::rustyline::error::ReadlineError;
use self::rustyline::Editor;
use self::rustyline::config::Configurer;

#[derive(Debug, PartialEq, Clone)]
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

pub struct Reader<'a> {
    re_number: Regex,
    re_string: Regex,
    re_char: Regex,
    re_symbol: Regex,
    use_stdin: bool,
    readline: Editor<()>,
    ps1: &'a str,
    ps2: &'a str,
    // 嵌套深度
    scope: isize,
    // 偷看
    lookahead: Option<Token>,
    // 字符串内部
    string: bool,
    // 当前行
    line: String,
    // 历史文件路径
    history: PathBuf,

}

impl Iterator for Reader<'_> {
    type Item = LispResult;
    fn next(&mut self) -> Option<Self::Item> {
        match self.read() {
            Ok(expr) => Some(Ok(expr)),
            Err(_) => None,
        }
    }
}

#[allow(dead_code)]
impl<'a> Reader<'a> {
    pub fn new() -> Self {
        let mut rl = Editor::<()>::new();
        let home_dir = dirs::home_dir().unwrap();
        let history = home_dir.join(".lagoon_history");
        if history.exists() {
            rl.load_history(&history).unwrap();
        }
        rl.set_auto_add_history(true);
        rl.set_history_ignore_dups(true);
        rl.set_history_ignore_space(true);
        Reader {
            re_string: Regex::new(r#"^"((\\.|[^"])*)""#).unwrap(),
            re_char: Regex::new(r"^#\\([[:alpha:]]+|.)").unwrap(), // 不包含\n
            re_number: Regex::new(r"^[-+]?\d+").unwrap(),
            re_symbol: Regex::new(r"^[^#;'`,\s()][^#;'`,\s()]*").unwrap(),
            use_stdin: false,
            ps1: "scheme> ",
            ps2: "",
            scope: 0,
            lookahead: None,
            string: false,
            readline: rl,
            line: String::new(),
            history,
        }
    }

    pub fn set_prompt(&mut self, ps1: &'a str, ps2: &'a str) {
        self.ps1 = ps1;
        self.ps2 = ps2;
    }

    pub fn set_stdio(&mut self, flag:bool) {
        self.use_stdin = flag;
    }

    pub fn read(&mut self) -> LispResult {
        use self::Sexp::*;

        self.scope = 0;
        self.string = false;

        let mut list_level = 0;
        let mut dot_count = 0;  // 当前列表中的dot个数
        let mut dot_stack = vec![];
        let mut current_exprs = vec![]; // 保存当前列表中的表达式
        let mut list_stack = vec![];
        let mut macros = vec![];
        let mut macro_stack = vec![];

        loop {
            let token = self.next_token()?;
            match token {
                Token::Quote => macros.push(Symbol("quote".to_owned())),
                Token::Quasiquote => macros.push(Symbol("quasiquote".to_owned())),
                Token::Unquote => macros.push(Symbol("unquote".to_owned())),
                Token::UnquoteSplicing => macros.push(Symbol("unquote-splicing".to_owned())),
                Token::Rune('(') => {
                    if list_level > 0 {
                        dot_stack.push(dot_count);
                        dot_count = 0;
                        list_stack.push(current_exprs.clone());
                        current_exprs.clear();
                    }
                    macro_stack.push(macros.clone());
                    macros.clear();
                    list_level += 1;
                }
                Token::Rune(')') => {
                    if list_level == 0 {
                        return self.parse_error(format!("unexpected `{}'", token).as_str());
                    }
                    list_level -= 1;

                    if let Some(m) = macro_stack.pop() {
                        macros = m;
                    }

                    let list = if current_exprs.is_empty() {
                        Nil
                    } else if dot_count > 0 {
                        let last = current_exprs.pop().unwrap();
                        // (. x) => x
                        if current_exprs.is_empty() {
                            last
                        } else {
                            List(current_exprs, Rc::new(last))
                        }
                    } else {
                        List(current_exprs, Rc::new(Nil))
                    };

                    if list_stack.is_empty() {
                        return Ok(Reader::expand_macro(&mut macros, &list));
                    }

                    let mut outer = list_stack.pop().unwrap();
                    outer.push(Reader::expand_macro(&mut macros, &list));
                    current_exprs = outer;
                    dot_count = dot_stack.pop().unwrap();
                }
                Token::Rune('.') => {
                    if list_level == 0 || dot_count > 0 || self.lookahead()? == Token::Rune(')') {
                        return self.parse_error("illegal use of `.'");
                    }
                    dot_count += 1
                }
                _ => {
                    if dot_count > 0 && self.lookahead()? != Token::Rune(')') {
                        return self.parse_error("illegal use of `.'");
                    }
                    let expr = self.read_atom(token)?;
                    if list_level == 0 {
                        return Ok(Reader::expand_macro(&mut macros, &expr));
                    }
                    current_exprs.push(Reader::expand_macro(&mut macros, &expr));
                }
            }
        }
    }

    // Expands reader macros
    fn expand_macro(macros: &mut Vec<Sexp>, inner: &Sexp) -> Sexp {
        use self::Sexp::*;
        let mut expr = inner.clone();
        while let Some(m) = macros.pop() {
            let init = vec![m, expr];
            expr = List(init, Rc::new(Nil));
        }
        expr
    }

    fn read_atom(&mut self, token: Token) -> LispResult {
        match token {
            Token::Number(lex) => self.parse_number(lex.as_str()),
            Token::Str(lex) => Ok(Sexp::Str(lex)),
            Token::Char(lex) => Ok(Sexp::Char(lex)),
            Token::Symbol(lex) => Ok(Sexp::Symbol(lex)),
            Token::Pound(lex) => self.parse_pound(lex.as_str()),
            _ => self.parse_error(format!("unexpected `{}'", token).as_str()),
        }
    }

    // 忽略空白和注释
    fn skip_whitespace(line: &str) -> String {
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
                    line.push('\n');
                    if !self.string {
                        line = Reader::skip_whitespace(&line);
                    }
                }
                Err(ReadlineError::Interrupted) => return Err(LispError::Interrupted),
                Err(ReadlineError::Eof) => return Err(LispError::EndOfInput),
                // TODO use a distinct error type
                Err(err) => panic!(err),
            };
        }
        self.readline.save_history(&self.history).unwrap();
        Ok(line.to_owned())
    }

    fn lookahead(&mut self) -> Result<Token, LispError> {
        if self.lookahead.is_some() {
            let token = self.lookahead.clone();
            Ok(token.unwrap())
        } else {
            let token = self.next_token()?;
            self.lookahead = Some(token.clone());
            Ok(token)
        }
    }

    fn next_token(&mut self) -> Result<Token, LispError> {
        use self::Token::*;

        if self.lookahead.is_some() {
            let token = self.lookahead.clone();
            self.lookahead = None;
            return Ok(token.unwrap());
        }

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
            return if &cap[0] == "." {
                Ok(Rune('.'))
            } else {
                Ok(Symbol(cap[0].to_lowercase()))
            };
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
                if let Some(next) = self.line.chars().next() {
                    self.line.remove(0);
                    return Ok(Pound(next.to_lowercase().to_string()));
                }
                return self.parse_error(format!("`{}'", first).as_str());
            }
            _ => return Ok(Rune(first)),
        }
    }

    // TODO 大整数
    fn parse_number(&mut self, lex: &str) -> LispResult {
        match lex.parse::<i64>() {
            Ok(n) => Ok(Sexp::Number(n)),
            Err(err) => self.parse_error(err.description()),
        }
    }

    // See also https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Additional-Notations.html
    fn parse_pound(&mut self, lex: &str) -> LispResult {
        match lex {
            "t" => Ok(Sexp::True),
            "f" => Ok(Sexp::False),
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
        Err(LispError::ReadError(err.to_owned()))
    }
}

// TODO 补充完整ASCII中所有的不可打印字符
// FIXME newline应该根据平台决定是linefeed还是return
// See also https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_6.html
fn name_to_char(name: &str) -> Option<char> {
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
fn char_to_name(ch: char) -> String {
    match ch {
        '\x08' => "backspace".to_owned(),
        '\n' => "newline".to_owned(),
        '\r' => "return".to_owned(),
        ' ' => "space".to_owned(),
        _ => ch.to_string(),
    }
}
