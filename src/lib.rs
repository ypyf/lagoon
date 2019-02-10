#![feature(test)]
#![feature(nll)]

extern crate test;

pub mod scheme;

#[cfg(test)]
mod tests {
    use test::Bencher;
    use scheme::interpreter::Interpreter;
    use scheme::value::Sexp::*;
    use scheme::error::LispError::*;

    #[test]
    fn it_works() {
        let mut interp = Interpreter::new();
        assert_eq!(Ok(Number(123)), interp.eval_string("(+ 1 2 (* 3 4 5 (- 6 -7 (/ 9 3) 8)))"));
        assert_eq!(Ok(True), interp.eval_string("(< 1 2)"));
        assert_eq!(Ok(False), interp.eval_string("(pair? '())"));
    }

    #[test]
    fn list() {
        let mut interp = Interpreter::new();
        assert_eq!(Ok(Nil), interp.eval_string("'()"));
        assert_eq!(Ok(Number(2)), interp.eval_string("'(. 2)"));
        assert_eq!(Ok(Symbol("a".to_string())), interp.eval_string("(car '(a))"));
        assert_eq!(Ok(Nil), interp.eval_string("(cdr '(a))"));
        assert_eq!(interp.eval_string("'(a b c . d)"), interp.eval_string("(cons 'a (cons 'b (cons 'c 'd)))"));
        assert_eq!(Ok(Number(3)), interp.eval_string("(car (cdr (car (cdr '(1 (2 3) 4 5)))))"));
        assert_eq!(interp.eval_string("'(1 2 3 quote (4 5 . 6))"), interp.eval_string("(cons 1 '(2 . (3 . '(4 5 . 6))))"));
    }

    #[test]
    fn lambda() {
        let mut interp = Interpreter::new();
        // equivalent to (list a b c)
        assert_eq!(Ok(List(Box::new(vec![Number(1), Number(2), Number(3), Nil]))), interp.eval_string("((lambda x x) 1 2 3)"));
        assert_eq!(Ok(Number(1)), interp.eval_string("((lambda (a b . x) a) 1 2 3)"));
        assert_eq!(Ok(Number(2)), interp.eval_string("((lambda (a b . x) b) 1 2 3)"));
        assert_eq!(Ok(List(Box::new(vec![Number(3), Nil]))), interp.eval_string("((lambda (a b . x) x) 1 2 3)"));
    }

    #[test]
    fn closure1() {
        let mut interp = Interpreter::new();
        interp.eval_string("(define (addn n) (lambda (x) (+ x n)))").unwrap();
        assert_eq!(Ok(Number(30)), interp.eval_string("((addn 10) 20)"));
        assert_eq!(Err(Undefined("x".to_owned())), interp.eval_string("x"));
        assert_eq!(Err(Undefined("n".to_owned())), interp.eval_string("n"));
    }

    #[test]
    fn closure2() {
        let mut interp = Interpreter::new();
        interp.eval_string("(define f (lambda (a) (+ a b)))").unwrap();
        interp.eval_string("(define b 10)").unwrap();
        assert_eq!(Ok(Number(30)), interp.eval_string("(f 20)"));
    }

    #[test]
    fn clsoure3() {
        let mut interp = Interpreter::new();
        interp.eval_string("(define func +)").unwrap();
        interp.eval_string("(define (foo func) (lambda (a b) (func a b)))").unwrap();
        assert_eq!(Ok(Number(2)), interp.eval_string("((foo *) 1 2)"));
    }

    #[test]
    fn closure4() {
        let mut interp = Interpreter::new();
        interp.eval_string("(define a 1)").unwrap();
        interp.eval_string("((lambda (x) (define a 2) (+ x a)) 2)").unwrap();
        assert_eq!(Ok(Number(1)), interp.eval_string("a"));
    }

    #[bench]
    fn bench_add_two(b: &mut Bencher) {
        let mut interp = Interpreter::new();
        b.iter(|| interp.eval_string("(+ 1 2)"));
    }

    #[bench]
    fn bench_is_pair(b: &mut Bencher) {
        let mut interp = Interpreter::new();
        b.iter(|| interp.eval_string("(pair? '())"));
    }
}
