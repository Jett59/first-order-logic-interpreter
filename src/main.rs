use std::io::Write;

use parser::Parser;

mod formula;
mod parser;

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdout().lock().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_string();
        if input.is_empty() {
            break;
        }
        let formula = Parser::parse(&input);
        println!("{:?}", formula);
    }
}
