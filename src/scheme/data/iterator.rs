use scheme::types::Sexp;
use scheme::types::Sexp::*;

#[derive(Debug, Clone)]
pub struct ListIntoIterator {
    state_stack: Vec<(usize, Sexp)>,
    index: usize,
    expr: Sexp,
}

impl ListIntoIterator {
    pub fn new(expr: Sexp) -> Self {
        ListIntoIterator {
            state_stack: vec![],
            index: 0,
            expr,
        }
    }
}

impl Iterator for ListIntoIterator {
    type Item = Sexp;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index >= self.expr.count() {
                if self.state_stack.is_empty() {
                    return None;
                } else {
                    let (index, expr) = self.state_stack.pop().unwrap();
                    self.index = index;
                    self.expr = expr;
                }
            }
            let index = self.index;
            let item = if let List(init, last) = &self.expr {
                if index >= init.len() {
                    (**last).clone()
                } else {
                    init[index].clone()
                }
            } else {
                self.expr.clone()
            };
            self.index += 1;
            match &item {
                List(_, _) => {
                    if self.index < self.expr.count() {
                        self.state_stack.push((self.index, self.expr.clone()));
                    }
                    self.index = 0;
                    self.expr = item;
                }
                _ => return Some(item),
            }
        }
    }
}

pub struct ListIterator<'a> {
    state_stack: Vec<(usize, &'a Sexp)>,
    index: usize,
    expr: &'a Sexp,
}

impl<'a> ListIterator<'a> {
    pub fn new(expr: &'a Sexp) -> Self {
        ListIterator {
            state_stack: vec![],
            index: 0,
            expr,
        }
    }
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = Sexp;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index >= self.expr.count() {
                if self.state_stack.is_empty() {
                    return None;
                } else {
                    let (index, expr) = self.state_stack.pop().unwrap();
                    self.index = index;
                    self.expr = expr
                }
            }
            let index = self.index;
            let item = if let List(init, last) = self.expr {
                if index >= init.len() {
                    &**last
                } else {
                    &init[index]
                }
            } else {
                self.expr
            };
            self.index += 1;
            match &item {
                List(_, _) => {
                    if self.index < self.expr.count() {
                        self.state_stack.push((self.index, self.expr));
                    }
                    self.index = 0;
                    self.expr = item;
                }
                _ => return Some(item.clone()),
            }
        }
    }
}

impl IntoIterator for Sexp {
    type Item = Sexp;
    type IntoIter = ListIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIntoIterator::new(self)
    }
}

impl<'a> IntoIterator for &'a Sexp {
    type Item = Sexp;
    type IntoIter = ListIterator<'a>;

    // 注意这里的self是引用
    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self)
    }
}
