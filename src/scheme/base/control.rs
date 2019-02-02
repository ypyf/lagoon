use scheme::types::Context;
use scheme::types::Sexp;
use scheme::types::LispResult;
use scheme::types::LispError::ArityMismatch;
use scheme::types::LispError::TypeMismatch;

pub fn apply(ctx: &mut Context, args: &[Sexp]) -> LispResult<Sexp> {
    use self::Sexp::*;
    let arity = args.len();
    if arity < 2 {
        return Err(ArityMismatch("apply".to_string(), 2, arity));
    }

    let mut arg_list = vec![];
    let (last, init) = args[1..].split_last().unwrap();
    if let List(mut xs) = last.clone() {
        if *xs.last().unwrap() != Nil {
            return Err(TypeMismatch("list".to_owned(), format!("{}", last)));
        }
        xs.pop();
        for arg in init {
            arg_list.push(arg.clone())
        }
        for arg in *xs {
            arg_list.push(arg)
        }
    } else if *last == Nil {
        for arg in init {
            arg_list.push(arg.clone())
        }
    } else {
        return Err(TypeMismatch("list".to_owned(), format!("{}", last)));
    };
    ctx.apply(&args[0], &arg_list)
}
