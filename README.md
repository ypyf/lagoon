# lagoon
A Scheme interpreter written in Rust

## Get Started

### Run in interactive mode

    ./lagoon
    
### Run program from a file (extension does not matter)

    ./lagoon myscript.scm
    
### Write yourself interpreter  
  
    pub mod scheme;

    use scheme::interpreter::Interpreter;

    fn main() {
        // Run interpreter in REPL mode
        Interpreter::new().run_repl();
        
        // Evaluate the expressions represented by a string
        Interpreter::new().eval_string("(+ 1 2 3)");
    }
    
