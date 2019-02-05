use scheme::context::Context;
use scheme::data::{ELLIPSIS, UNDERSCORE, LispResult};
use scheme::data::error::LispError::{AssignError, BadSyntax};
use scheme::data::value::Sexp::*;
use scheme::data::value::{Sexp, SyntaxRule, Transformer};

use std::collections::{HashSet, VecDeque};
use std::rc::Rc;


pub fn quote(_context: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 1 {
        return Err(BadSyntax("quote".to_string(), None));
    }
    Ok(exprs[0].clone())
}

// FIXME (define a (define a 1)) => define: not allowed in an expression context
// definition in expression context, where definitions are not allowed, in from (define a 1)
pub fn define(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity == 0 {
        return ctx.syntax_error("define");
    } else if arity == 1 {
        return Err(BadSyntax("define".to_owned(), Some("missing expression after identifier".to_owned())));
    } else if arity > 2 {
        return Err(BadSyntax("define".to_owned(), Some("multiple expressions after identifier".to_owned())));
    }

    match &exprs[0] {
        Symbol(sym) => {
            let val = ctx.eval(&exprs[1])?;
            match val {
                Closure { name: _, params, vararg, body, env } => {
                    let closure = Closure { name: sym.clone(), params, vararg, body, env };
                    ctx.insert(sym, &closure);
                }
                _ => ctx.insert(sym, &val)
            }
        }
        List(xs) => match xs.last().unwrap() {
            // (define (name x y) (+ x y)) => (define name (lambda (x y) (+ x y)))
            Nil => {
                let expr = if let Some((name, params)) = xs.split_first() {
                    let params = List(Box::new(params.to_vec()));
                    let lambda = List(Box::new(vec![Symbol("lambda".to_owned()), params, exprs[1].clone(), Nil]));
                    List(Box::new(vec![Symbol("define".to_owned()), name.clone(), lambda, Nil]))
                } else {
                    return ctx.syntax_error("define");
                };
                ctx.eval(&expr)?;
            }
            // (define (name x y . v) (+ x y)) => (define name (lambda (x y . v) (+ x y)))
            Symbol(_) => {
                let expr = if let Some((name, params)) = xs.split_first() {
                    let params = List(Box::new(params.to_vec()));
                    let lambda = List(Box::new(vec![Symbol("lambda".to_owned()), params, exprs[1].clone(), Nil]));
                    List(Box::new(vec![Symbol("define".to_owned()), name.clone(), lambda, Nil]))
                } else {
                    return ctx.syntax_error("define");
                };
                ctx.eval(&expr)?;
            }
            _ => return ctx.syntax_error("define"),
        }
        _ => return ctx.syntax_error("define"),
    }
    Ok(Void)
}

pub fn assign(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 2 {
        return Err(BadSyntax("set!".to_owned(), Some(format!("has {} parts after keyword", arity))));
    }

    match &exprs[0] {
        Symbol(sym) => {
            let val = ctx.eval(&exprs[1])?;
            if ctx.assign(sym, &val) {
                Ok(Void)
            } else {
                return Err(AssignError("cannot set undefined".to_owned()));
            }
        }
        _ => return Err(AssignError("not an identifier".to_owned())),
    }
}

// FIXME body不能为空
pub fn lambda(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity == 0 {
        return ctx.syntax_error("lambda");
    } else if arity == 1 {
        return Err(BadSyntax("body".to_owned(), Some("no expression in body".to_owned())));
    }

    let (formals, body) = exprs.split_first().unwrap();
    match formals {
        Nil => Ok(Closure {
            name: String::new(),
            params: vec![],
            vararg: None,
            body: Rc::new(body.to_vec()),
            env: ctx.clone(),
        }),
        List(xs) => {
            let (last, init) = xs.split_last().unwrap();
            let vararg = match last {
                Nil => None,
                Symbol(ident) => Some(ident.clone()),
                _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()))),
            };

            let mut params = vec![];
            for expr in init {
                match expr {
                    Symbol(ident) => params.push(ident.clone()),
                    _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()))),
                }
            }
            Ok(Closure {
                name: String::new(),
                params,
                vararg,
                body: Rc::new(body.to_vec()),
                env: ctx.clone(),
            })
        }
        Symbol(ident) => {
            Ok(Closure {
                name: String::new(),
                params: vec![],
                vararg: Some(ident.clone()),
                body: Rc::new(body.to_vec()),
                env: ctx.clone(),
            })
        }
        _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()))),
    }
}

// FIXME tail call optimization
pub fn if_exp(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity < 2 || arity > 3 {
        return ctx.syntax_error("if");
    }

    if arity == 2 {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        if ctx.eval(pred)?.is_true() {
            ctx.eval(conseq)
        } else {
            Ok(Void)
        }
    } else {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        let alt = &exprs[2];
        if ctx.eval(pred)?.is_true() {
            ctx.eval(conseq)
        } else {
            ctx.eval(alt)
        }
    }
}

pub fn define_syntax(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 2 {
        return ctx.syntax_error("define-syntax");
    }

    match &exprs[0] {
        Symbol(keyword) => {
            if let Some(datum) = check_transformer(&exprs[1]) {
                if datum.is_empty() {
                    Err(BadSyntax("define-syntax".to_owned(), Some("only a `syntax-rules' form is allowed".to_owned())))
                } else {
                    let transformer = syntax_rules(ctx, datum)?;
                    ctx.insert(keyword, &DefineSyntax { keyword: keyword.clone(), transformer });
                    Ok(Void)
                }
            } else {
                Err(BadSyntax("define-syntax".to_owned(), Some("only a `syntax-rules' form is allowed".to_owned())))
            }
        }
        _ => ctx.syntax_error("define-syntax"),
    }
}

// TODO 合并到syntax_rules
// Check (syntax-rules ...) form out
fn check_transformer(form: &Sexp) -> Option<&[Sexp]> {
    if form.is_list() {
        if let List(xs) = form {
            if let Symbol(ident) = xs.first().unwrap() {
                if ident == "syntax-rules" {
                    return form.rest(1);
                }
            }
        }
    }
    None
}

fn syntax_rules(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Transformer> {
    let arity = exprs.len();

    let first = &exprs[0];

    // (syntax-rules (<identifier>*))
    if arity == 1 {
        match first {
            Nil => return Ok(Transformer { id: None, literals: vec![], rules: vec![] }),
            List(_) => if first.is_list() {
                let mut id_list = vec![];
                for item in first.init().unwrap() {
                    if let Symbol(ident) = item {
                        if ident != UNDERSCORE && ident != ELLIPSIS {
                            id_list.push(ident.clone())
                        } else {
                            return ctx.syntax_error1("syntax-rules", "invalid literals list");
                        }
                    } else {
                        return ctx.syntax_error("syntax-rules");
                    }
                }
                return Ok(Transformer { id: None, literals: id_list, rules: vec![] });
            } else {
                return ctx.syntax_error("syntax-rules");
            }
            _ => return ctx.syntax_error("syntax-rules"),
        }
    }

    let second = &exprs[1];

    match first {
        // (syntax-rules <identifier> (<identifier>*) <syntax rules>*)
        Symbol(sym) => match second {
            Nil => {
                let expr = Transformer {
                    id: None,
                    literals: vec![],
                    rules: parse_syntax_rules(ctx, &exprs[2..])?,
                };
                Ok(expr)
            }
            List(_) => {
                let rules = if arity == 2 {
                    vec![]
                } else {
                    parse_syntax_rules(ctx, &exprs[2..])?
                };
                if first.is_list() {
                    let mut id_list = vec![];
                    for item in first.init().unwrap() {
                        if let Symbol(ident) = item {
                            if ident != UNDERSCORE && ident != ELLIPSIS {
                                id_list.push(ident.clone())
                            } else {
                                return ctx.syntax_error1("syntax-rules", "invalid literals list");
                            }
                        } else {
                            return ctx.syntax_error("syntax-rules");
                        }
                    }
                    Ok(Transformer { id: Some(sym.clone()), literals: id_list, rules })
                } else {
                    ctx.syntax_error("syntax-rules")
                }
            }
            _ => ctx.syntax_error("syntax-rules"),
        }
        // (syntax-rules (<identifier>*) <syntax rules>+)
        Nil => {
            let expr = Transformer {
                id: None,
                literals: vec![],
                rules: parse_syntax_rules(ctx, &exprs[1..])?,
            };
            Ok(expr)
        }
        List(_) => if first.is_list() {
            let mut id_list = vec![];
            for item in first.init().unwrap() {
                if let Symbol(ident) = item {
                    if ident != UNDERSCORE && ident != ELLIPSIS {
                        id_list.push(ident.clone())
                    } else {
                        return ctx.syntax_error1("syntax-rules", "invalid literals list");
                    }
                } else {
                    return ctx.syntax_error("syntax-rules");
                }
            }
            Ok(Transformer { id: None, literals: id_list, rules: parse_syntax_rules(ctx, &exprs[1..])? })
        } else {
            ctx.syntax_error("syntax-rules")
        }
        _ => ctx.syntax_error("syntax-rules"),
    }
}

// Parse (pattern template) clause
fn parse_syntax_rules(ctx: &Context, exprs: &[Sexp]) -> LispResult<Vec<SyntaxRule>> {
    let mut rules = vec![];
    for expr in exprs {
        let (pattern, template) = check_syntax_rules_clause(ctx, expr)?;
        rules.push(SyntaxRule { pattern, template });
    }
    Ok(rules)
}

// Check (pattern template) clause
fn check_syntax_rules_clause(ctx: &Context, clause: &Sexp) -> LispResult<(Sexp, Sexp)> {
    if let List(xs) = clause {
        if !clause.is_list() || clause.list_count() != 2 {
            let error = format!("invalid syntax-rules clause {}", clause);
            return ctx.syntax_error1("syntax-rules", &error);
        }
        let pattern = compile_pattern(ctx, clause, &xs[0])?;
// TODO check template
        let template = xs[1].clone();
        Ok((pattern, template))
    } else {
        let error = format!("invalid syntax-rules clause {}", clause);
        return ctx.syntax_error1("syntax-rules", &error);
    }
}

fn compile_pattern(ctx: &Context, clause: &Sexp, pattern: &Sexp) -> LispResult<Sexp> {
    let mut compiled_pattern = if let List(xs) = pattern {
        if xs.first().unwrap().is_symbol() {
            pattern.clone()
        } else {
            // 最外层模式不能以非标示符开头
            let error = format!("invalid syntax-rules clause {}", clause);
            return ctx.syntax_error1("syntax-rules", &error);
        }
    } else {
        // 最外层模式必须是列表结构
        let error = format!("invalid syntax-rules clause {}", clause);
        return ctx.syntax_error1("syntax-rules", &error);
    };

    // 忽略最外层模式的第一个元素，因为它总是假定为命名这个语法规则的关键字
    compiled_pattern.set_nth(0, &Symbol("_".to_string()));

    let mut q = VecDeque::new();
    q.push_back(&compiled_pattern);
    let mut idents = HashSet::new();
    loop {
        let pat = if let Some(pat) = q.pop_front() {
            pat
        } else {
            return Ok(compiled_pattern);
        };

        let mut ellipsis = 0;
        for (index, expr) in pat.as_slice().unwrap().iter().enumerate() {
            if let Symbol(ident) = expr {
                if ident == ELLIPSIS {
                    if ellipsis > 0 || index == 0 {
                        let error = format!("misplaced ellipsis in pattern {}", compiled_pattern);
                        return ctx.syntax_error1("syntax-rules", &error);
                    }
                    ellipsis += 1;
                } else if idents.contains(&ident) {
                    let error = format!("duplicate pattern variable {} in {}", ident, compiled_pattern);
                    return ctx.syntax_error1("syntax-rules", &error);
                } else if ident != UNDERSCORE {
                    idents.insert(ident);
                }
            } else if expr.is_pair() {
                q.push_back(expr)
            }
        }
    }
}
