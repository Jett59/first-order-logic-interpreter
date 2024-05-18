use std::io::Write;

use crate::parser::Parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Quantifier {
    ForAll,
    ThereExists,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOperator {
    And,
    Or,
    Implies,
    EquivalentTo,
}

#[derive(Debug, Clone)]
enum Expression {
    QuantifierChain(Vec<(Quantifier, String)>, Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
    Predicate(String, Vec<String>),
    Negation(Box<Expression>),
    Literal(bool),
}

mod parser;

fn main() {
    loop {
        let mut input = String::new();
        print!("Enter a formula: ");
        std::io::stdout().lock().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_string();
        if input.is_empty() {
            break;
        }
        println!("{:?}", Parser::new().parse(input));
    }
}