use scheme::data::value::Sexp;

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
            if self.index >= self.expr.list_count() {
                if self.state_stack.is_empty() {
                    return None;
                } else {
                    let (index, expr) = self.state_stack.pop().unwrap();
                    self.index = index;
                    self.expr = expr;
                }
            }
            let index = self.index;
            let item = if let Some(item) = self.expr.nth(index) {
                item.clone()
            } else {
                self.expr.clone()
            };
            self.index += 1;
            if item.is_pair() {
                if self.index < self.expr.list_count() {
                    self.state_stack.push((self.index, self.expr.clone()));
                }
                self.index = 0;
                self.expr = item;
            } else {
                return Some(item)
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
    type Item = &'a Sexp;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index >= self.expr.list_count() {
                if self.state_stack.is_empty() {
                    return None;
                } else {
                    let (index, expr) = self.state_stack.pop().unwrap();
                    self.index = index;
                    self.expr = expr
                }
            }
            let index = self.index;
            let item = if let Some(item) = self.expr.nth(index) {
                item
            } else {
                self.expr
            };
            self.index += 1;
            if item.is_pair() {
                if self.index < self.expr.list_count() {
                    self.state_stack.push((self.index, self.expr));
                }
                self.index = 0;
                self.expr = item;
            } else {
                return Some(item)
            }
        }
    }
}

//pub struct ListIteratorMut<'a> {
//    state_stack: Vec<(usize, &'a mut Sexp)>,
//    index: usize,
//    expr: &'a mut Sexp,
//}
//
//impl<'a> ListIteratorMut<'a> {
//    pub fn new(expr: &'a mut Sexp) -> Self {
//        ListIteratorMut {
//            state_stack: vec![],
//            index: 0,
//            expr,
//        }
//    }
//}
