use scheme::context::Context;
use scheme::{ELLIPSIS, UNDERSCORE, LispResult};
use scheme::error::LispError::BadSyntax;
use scheme::value::Sexp::*;
use scheme::value::{Sexp, SyntaxRule, Transformer};

use std::collections::{HashSet, VecDeque};

pub fn define_syntax(ctx: &mut Context, exprs: &[Sexp]) -> LispResult<Sexp> {
    let arity = exprs.len();
    if arity != 2 {
        syntax_error!("define-syntax", "bad syntax");
    }

    match &exprs[0] {
        Symbol(keyword) => {
            if let Some(datum) = check_transformer(&exprs[1]) {
                if datum.is_empty() {
                    syntax_error!("define-syntax", "only a `syntax-rules' form is allowed")
                } else {
                    let transformer = syntax_rules(ctx, datum)?;
                    ctx.insert(keyword, &DefineSyntax { keyword: keyword.clone(), transformer });
                    Ok(Void)
                }
            } else {
                syntax_error!("define-syntax", "only a `syntax-rules' form is allowed");
            }
        }
        _ => syntax_error!("define-syntax", "bad syntax"),
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
                            syntax_error!("syntax-rules", "invalid literals list");
                        }
                    } else {
                        syntax_error!("syntax-rules", "bad syntax");
                    }
                }
                return Ok(Transformer { id: None, literals: id_list, rules: vec![] });
            } else {
                syntax_error!("syntax-rules", "bad syntax");
            }
            _ => syntax_error!("syntax-rules", "bad syntax"),
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
                                syntax_error!("syntax-rules", "invalid literals list");
                            }
                        } else {
                            syntax_error!("syntax-rules", "bad syntax");
                        }
                    }
                    Ok(Transformer { id: Some(sym.clone()), literals: id_list, rules })
                } else {
                    syntax_error!("syntax-rules", "bad syntax");
                }
            }
            _ => syntax_error!("syntax-rules", "bad syntax"),
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
                        syntax_error!("syntax-rules", "invalid literals list");
                    }
                } else {
                    syntax_error!("syntax-rules", "bad syntax");
                }
            }
            Ok(Transformer { id: None, literals: id_list, rules: parse_syntax_rules(ctx, &exprs[1..])? })
        } else {
            syntax_error!("syntax-rules", "bad syntax");
        }
        _ => syntax_error!("syntax-rules", "bad syntax"),
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
    let error = format!("invalid syntax-rules clause {}", clause);
    if let List(xs) = clause {
        if !clause.is_list() || clause.list_count() != 2 {
            syntax_error!("syntax-rules", error);
        }
        let pattern = compile_pattern(ctx, clause, &xs[0])?;
        // TODO check template
        let template = xs[1].clone();
        Ok((pattern, template))
    } else {
        syntax_error!("syntax-rules", error);
    }
}

fn compile_pattern(_ctx: &Context, clause: &Sexp, pattern: &Sexp) -> LispResult<Sexp> {
    let error = format!("invalid syntax-rules clause {}", clause);
    let mut compiled_pattern = if let List(xs) = pattern {
        if xs.first().unwrap().is_symbol() {
            pattern.clone()
        } else {
            // 最外层模式不能以非标示符开头
            syntax_error!("syntax-rules", error);
        }
    } else {
        // 最外层模式必须是列表结构
        syntax_error!("syntax-rules", error);
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
                        syntax_error!("syntax-rules", error);
                    }
                    ellipsis += 1;
                } else if idents.contains(&ident) {
                    let error = format!("duplicate pattern variable {} in {}", ident, compiled_pattern);
                    syntax_error!("syntax-rules", error);
                } else if ident != UNDERSCORE {
                    idents.insert(ident);
                }
            } else if expr.is_pair() {
                q.push_back(expr)
            }
        }
    }
}
