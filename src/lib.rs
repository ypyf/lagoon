#![feature(test)]
#![feature(nll)]

extern crate test;

pub mod scheme;

#[cfg(test)]
mod tests {
    use test::Bencher;
    use scheme::types::Sexp::*;
    use scheme::interpreter::Interpreter;

    #[test]
    fn it_works() {
        let mut interp = Interpreter::new();
        assert_eq!(Ok(Number(3)), interp.eval_string("(+ 1 2)"));
        assert_eq!(Ok(Number(2)), interp.eval_string("(* 1 2)"));
        assert_eq!(Ok(True), interp.eval_string("(< 1 2)"));
        assert_eq!(Ok(False), interp.eval_string("(pair? '())"));
        interp.eval_string("(define (addn n) (lambda (x) (+ x n)))").unwrap();
        assert_eq!(Ok(Number(30)), interp.eval_string("((addn 10) 20)"));
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
