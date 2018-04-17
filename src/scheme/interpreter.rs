use scheme::primitives;
use scheme::reader::Reader;
use scheme::types::Context;
use scheme::types::Sexp;

pub struct Interpreter<'a> {
    reader: Reader<'a>,
    context: Context,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Interpreter {
            reader: Reader::new("r5rs> "),
            context: Context::new(),
        }
    }

    fn set_globals(&mut self) {
        self.context.define_procedure("+", primitives::plus);
        self.context.define_procedure("-", primitives::subtract);
        self.context.define_procedure("*", primitives::mul);
        self.context.define_synatx("define", primitives::define);
        self.context.define_synatx("quote", primitives::quote);
    }

    pub fn run_repl(&mut self) {
        self.context.enter_scope();
        self.set_globals();
        loop {
            match self.reader.read() {
                Ok(sexp) => match self.context.eval(&sexp) {
                    Ok(Sexp::Void) => (),
                    Ok(val) => println!("{}", val),
                    Err(err) => println!("{}", err),
                },
                Err(err) => println!("{}", err),
            }
        }
    }
}
