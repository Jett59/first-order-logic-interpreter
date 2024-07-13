use std::io::Write;

use formula::ParsedFormulaInterpreter;
use parser::Parser;

mod formula;
mod parser;

fn main() {
    loop {
        let mut input = String::new();
        print!("Φ: ");
        std::io::stdout().lock().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_string();
        if input.is_empty() {
            break;
        }
        let first = match Parser::parse(&input) {
            Ok(formula) => formula,
            Err(e) => {
                println!("Error: {}", e);
                continue;
            }
        };
        println!("{:?}", first);
        let mut input = String::new();
        print!("Ψ: ");
        std::io::stdout().lock().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_string();
        if input.is_empty() {
            break;
        }
        let second = match Parser::parse(&input) {
            Ok(formula) => formula,
            Err(e) => {
                println!("Error: {}", e);
                continue;
            }
        };
        println!("{:?}", second);
        let mut interpreter = ParsedFormulaInterpreter::default();
        let first = interpreter.interpret(first).compacted();
        let second = interpreter.interpret(second).compacted();
        println!("Φ: {:?}", first);
        println!("Ψ: {:?}", second);
        println!("Equivalent: {}", first.equivalent_to(&second));
    }
}
