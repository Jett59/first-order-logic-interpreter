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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expression {
    QuantifierChain(Vec<(Quantifier, String)>, Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
    Predicate(String, Vec<Expression>),
    Object(String),
    Function(String, Vec<Expression>),
    Negation(Box<Expression>),
}

mod parser;

fn main() {
    loop {
        let mut input = String::new();
        print!("Axioms file name: ");
        std::io::stdout().lock().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_string();
        if input.is_empty() {
            break;
        }
        // The axioms file has each line as an axiom
        let axioms = std::fs::read_to_string(&input).unwrap();
        let axioms = axioms
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| Parser::new().parse(line.to_string()))
            .collect::<Vec<_>>();
        println!("{:?}", axioms);
    }
}
