# lagoon
A Scheme interpreter written in Rust

## Get Started

### Execute in interactive mode

    ./lagoon
    
### Execute a Scheme script (extension does not matter)

    ./lagoon myscript.scm
    
### Embed Lagoon in Rust application  
  
    pub mod scheme;

    use scheme::interpreter::Interpreter;

    fn main() {
        // Create an interpreter object
        let mut interp = Interpreter::new();
        
        // Evaluate the expressions represented by a string
        interp.eval_string("(+ 1 2 3)");
                
        // Run interpreter in REPL mode
        interp.run_repl();
    }
    
