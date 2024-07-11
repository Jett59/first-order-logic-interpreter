use std::{iter::Peekable, str::Chars};

use crate::formula::{LogicalOperatorName, ParsedFormula};

struct Tokenizer<Iter>
where
    Iter: Iterator<Item = char>,
{
    characters: Peekable<Iter>,
}

impl<Iter> Tokenizer<Iter>
where
    Iter: Iterator<Item = char>,
{
    fn new(characters: Iter) -> Self {
        Tokenizer {
            characters: characters.peekable(),
        }
    }
}

impl<Iter> Iterator for Tokenizer<Iter>
where
    Iter: Iterator<Item = char>,
{
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = String::new();
        while let Some(character) = self.characters.peek() {
            if character.is_whitespace() {
                if !token.is_empty() {
                    return Some(token);
                } else {
                    self.characters.next();
                }
            } else if character.is_alphanumeric() || character == &'_' {
                token.push(self.characters.next().unwrap());
            } else {
                if !token.is_empty() {
                    return Some(token);
                } else {
                    return Some(self.characters.next().unwrap().to_string());
                }
            }
        }
        if !token.is_empty() {
            Some(token)
        } else {
            None
        }
    }
}

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<Chars<'a>>>,
}

impl<'a> Parser<'a> {
    fn new(expression: &'a str) -> Self {
        Parser {
            tokenizer: Tokenizer::new(expression.chars()).peekable(),
        }
    }

    pub fn parse(expression: &'a str) -> Result<ParsedFormula, String> {
        let mut parser = Parser::new(expression);
        let result = parser.parse_formula();
        if parser.tokenizer.peek().is_some() {
            Err(format!(
                "Unexpected token: {}",
                parser.tokenizer.next().unwrap()
            ))
        } else {
            result
        }
    }

    fn parse_formula(&mut self) -> Result<ParsedFormula, String> {
        let next_token = self.tokenizer.next().ok_or("Unexpected end of input")?;
        if next_token == "¬" {
            let formula = self.parse_formula()?;
            Ok(ParsedFormula::Compound(
                LogicalOperatorName::Not,
                vec![formula],
            ))
        } else if next_token == "(" {
            let left = self.parse_formula()?;
            let operator = self.tokenizer.next().ok_or("Unexpected end of input")?;
            let right = self.parse_formula()?;
            if self.tokenizer.peek() != Some(&")".to_string()) {
                return Err(format!(
                    "Expected ')', found: {}",
                    self.tokenizer.next().unwrap()
                ));
            }
            self.tokenizer.next();
            if let Ok(logical_operator) = LogicalOperatorName::try_from(operator.as_str()) {
                Ok(ParsedFormula::Compound(logical_operator, vec![left, right]))
            } else {
                Err(format!("Invalid logical operator: {}", operator))
            }
        } else if next_token == "∀" {
            let variable = self.tokenizer.next().ok_or("Unexpected end of input")?;
            let formula = self.parse_formula()?;
            Ok(ParsedFormula::Universal(variable, Box::new(formula)))
        } else if next_token == "∃" {
            let variable = self.tokenizer.next().ok_or("Unexpected end of input")?;
            let formula = self.parse_formula()?;
            Ok(ParsedFormula::Existential(variable, Box::new(formula)))
        } else {
            // The only kind of atomic formula we handle is the binary atomic formula (eg "x∈y")
            let left = next_token;
            let operator = self.tokenizer.next().ok_or("Unexpected end of input")?;
            let right = self.tokenizer.next().ok_or("Unexpected end of input")?;
            Ok(ParsedFormula::Atomic(operator, vec![left, right]))
        }
    }
}
