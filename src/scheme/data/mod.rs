use self::error::LispError;

pub mod error;
pub mod value;
pub mod iterator;

pub const UNDERSCORE: &str = "_";
pub const ELLIPSIS: &str = "...";

pub type LispResult<T> = Result<T, LispError>;

