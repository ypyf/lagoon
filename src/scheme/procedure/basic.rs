use scheme::types::Context;
use scheme::types::LispError::*;
use scheme::types::LispResult;
use scheme::types::SyntaxRule;
use scheme::types::Transformer;
use scheme::types::Sexp;
use scheme::types::Sexp::*;
use scheme::types::{UNDERSCORE, ELLIPSIS};

use std::rc::Rc;
use std::collections::HashSet;

pub fn quote(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 1 {
        return ctx.syntax_error("quote");
    }
    Ok(exprs[0].clone())
}

// FIXME (define a (define a 1)) => define: not allowed in an expression context
// definition in expression context, where definitions are not allowed, in from (define a 1)
pub fn define(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult<Sexp> {
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
                Closure { name: _, params, vararg, body, context } => {
                    let closure = Closure { name: sym.clone(), params, vararg, body, context };
                    ctx.bind(sym, &closure);
                }
                _ => ctx.bind(sym, &val)
            }
        }
        List(init, last) => match **last {
            // (define (name x y) (+ x y)) => (define name (lambda (x y) (+ x y)))
            Nil => {
                let expr = if let Some((name, params)) = init.split_first() {
                    let param_list = List(params.to_vec(), Rc::new(Nil));
                    let lambda = List(vec![Symbol("lambda".to_owned()), param_list, exprs[1].clone()], Rc::new(Nil));
                    List(vec![Symbol("define".to_owned()), name.clone(), lambda], Rc::new(Nil))
                } else {
                    return ctx.syntax_error("define");
                };
                ctx.eval(&expr)?;
            }
            // (define (name x y . v) (+ x y)) => (define name (lambda (x y . v) (+ x y)))
            Symbol(_) => {
                let expr = if let Some((name, params)) = init.split_first() {
                    let param_list = List(params.to_vec(), last.clone());
                    let lambda = List(vec![Symbol("lambda".to_owned()), param_list, exprs[1].clone()], Rc::new(Nil));
                    List(vec![Symbol("define".to_owned()), name.clone(), lambda], Rc::new(Nil))
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

pub fn assign(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult<Sexp> {
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
                return Err(AssignError("cannot set undefined".to_owned(), ctx.clone()));
            }
        }
        _ => return Err(AssignError("not an identifier".to_owned(), ctx.clone())),
    }
}

// FIXME body不能为空
pub fn lambda(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity == 0 {
        return ctx.syntax_error("lambda");
    } else if arity == 1 {
        return Err(BadSyntax("body".to_owned(), Some("no expression in body".to_owned())));
    }

    match &exprs[0] {
        Nil => Ok(Closure {
            name: String::new(),
            params: vec![],
            vararg: None,
            body: exprs[1..].to_vec(),
            context: (*ctx).clone(),
        }),
        List(init, last) => {
            let vararg = match **last {
                Nil => None,
                Symbol(ref sym) => Some(sym.clone()),
                _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()))),
            };
            let mut params = vec![];
            for expr in init {
                match expr {
                    Symbol(sym) => params.push(sym.clone()),
                    _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()))),
                }
            }
            Ok(Closure {
                name: String::new(),
                params,
                vararg,
                body: exprs[1..].to_vec(),
                context: (*ctx).clone(),
            })
        }
        Symbol(sym) => {
            Ok(Closure {
                name: String::new(),
                params: vec![],
                vararg: Some(sym.clone()),
                body: exprs[1..].to_vec(),
                context: (*ctx).clone(),
            })
        }
        _ => return Err(BadSyntax("lambda".to_owned(), Some("not an identifier".to_owned()))),
    }
}

pub fn if_exp(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity < 2 || arity > 3 {
        return ctx.syntax_error("if");
    }

    if arity == 2 {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        if ctx.eval(pred)?.is_true() {
            ctx.eval(conseq)    // FIXME 尾部
        } else {
            Ok(Void)
        }
    } else {
        let pred = &exprs[0];
        let conseq = &exprs[1];
        let alt = &exprs[2];
        if ctx.eval(pred)?.is_true() {
            ctx.eval(conseq)    // FIXME 尾部
        } else {
            ctx.eval(alt)       // FIXME 尾部
        }
    }
}

pub fn define_syntax(ctx: &mut Context, exprs: Vec<Sexp>) -> LispResult<Sexp> {
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
                    ctx.bind(keyword, &Syntax { keyword: keyword.clone(), transformer });
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
// Check (syntax-rules) form
fn check_transformer(expr: &Sexp) -> Option<&[Sexp]> {
    if expr.is_list() {
        if let List(init, _) = expr {
            if init[0] == Symbol("syntax-rules".to_string()) {
                return Some(&init[1..]);
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
            List(init, last) => if **last == Nil {
                let mut id_list = vec![];
                for item in init {
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
            List(init, last) => {
                let rules = if arity == 2 {
                    vec![]
                } else {
                    parse_syntax_rules(ctx, &exprs[2..])?
                };
                if **last == Nil {
                    let mut id_list = vec![];
                    for item in init {
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
        List(init, last) => if **last == Nil {
            let mut id_list = vec![];
            for item in init {
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
    if !clause.is_list() || !clause.is_pair() || clause.count() != 2 {
        let error = format!("invalid syntax-rules clause {}", clause);
        return ctx.syntax_error1("syntax-rules", &error);
    }

    let list = clause.to_vec();
    let mut pattern = list[0].clone();
    let mut template = list[1].clone();

    if !pattern.is_pair() || !pattern.first().is_symbol() {
        let error = format!("invalid syntax-rules clause {}", clause);
        return ctx.syntax_error1("syntax-rules", &error);
    }

    *pattern.first_mut() = Symbol("_".to_string());
    let mut idents = HashSet::new();
    for expr in &pattern {
        if let Symbol(ident) = expr {
            if ident != UNDERSCORE && idents.contains(&ident) {
                let error = if ident == ELLIPSIS {
                    format!("misplaced ellipsis in pattern {}", pattern)
                } else {
                    format!("duplicate pattern variable {} in {}", ident, pattern)
                };
                return ctx.syntax_error1("syntax-rules", &error);
            }
            idents.insert(ident);
        }
    }

    Ok((pattern, template))
}
