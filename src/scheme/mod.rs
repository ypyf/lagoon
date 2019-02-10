macro_rules! syntax_error {
    ($($arg:tt)*) => (
        return Err(BadSyntax(format!("{}: {}", $($arg)*)))
    )
}

pub mod interpreter;
pub mod value;
pub mod error;
mod data;
mod base;
mod reader;
mod env;
mod machine;

use scheme::error::LispError;

pub const UNDERSCORE: &str = "_";
pub const ELLIPSIS: &str = "...";

pub type LispResult<T> = Result<T, LispError>;
