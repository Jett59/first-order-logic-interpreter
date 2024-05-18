use std::iter::Peekable;

use crate::{BinaryOperator, Expression, Quantifier};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Quantifier(Quantifier),
    BinaryOperator(BinaryOperator),
    Negation,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Identifier(String),
}

struct Tokenizer {
    remaining_text: String,
}

impl Tokenizer {
    fn new(text: String) -> Tokenizer {
        Tokenizer {
            remaining_text: text,
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_text.is_empty() {
            return None;
        }

        let first_char = self.remaining_text.chars().next().unwrap();
        match first_char {
            ' ' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                self.next()
            }
            'f' | 'F'
                if self
                    .remaining_text
                    .to_ascii_lowercase()
                    .starts_with("for all") =>
            {
                self.remaining_text = self.remaining_text["for all".len()..].to_string();
                Some(Token::Quantifier(Quantifier::ForAll))
            }
            't' | 'T'
                if self
                    .remaining_text
                    .to_ascii_lowercase()
                    .starts_with("there exists") =>
            {
                self.remaining_text = self.remaining_text["there exists".len()..].to_string();
                Some(Token::Quantifier(Quantifier::ThereExists))
            }
            '&' if self.remaining_text.starts_with("&&") => {
                self.remaining_text = self.remaining_text[2..].to_string();
                Some(Token::BinaryOperator(BinaryOperator::And))
            }
            '|' if self.remaining_text.starts_with("||") => {
                self.remaining_text = self.remaining_text[2..].to_string();
                Some(Token::BinaryOperator(BinaryOperator::Or))
            }
            '<' if self.remaining_text.starts_with("<->") => {
                self.remaining_text = self.remaining_text[3..].to_string();
                Some(Token::BinaryOperator(BinaryOperator::EquivalentTo))
            }
            '-' if self.remaining_text.starts_with("->") => {
                self.remaining_text = self.remaining_text[2..].to_string();
                Some(Token::BinaryOperator(BinaryOperator::Implies))
            }
            '!' | '~' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::Negation)
            }
            '(' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::OpenParen)
            }
            ')' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::CloseParen)
            }
            '{' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::OpenBrace)
            }
            '}' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::CloseBrace)
            }
            ',' => {
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::Comma)
            }
            _ => {
                let mut identifier = String::new();
                for c in self.remaining_text.chars() {
                    if c.is_alphanumeric() || c == '_' {
                        identifier.push(c);
                    } else {
                        break;
                    }
                }
                if identifier.len() > 0 {
                    self.remaining_text = self.remaining_text[identifier.len()..].to_string();
                    Some(Token::Identifier(identifier))
                } else {
                    panic!("Unexpected character: {:?}", first_char);
                }
            }
        }
    }
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(&self, text: String) -> Expression {
        let mut tokenizer = Tokenizer::new(text).peekable();
        self.parse_expression(&mut tokenizer, None)
    }

    fn parse_expression(
        &self,
        tokenizer: &mut Peekable<Tokenizer>,
        close: Option<Token>,
    ) -> Expression {
        // Expressions are strings of 'atomic' expressions separated with binary operators.
        // First, we just parse all of the atomic expressions. Then we go through and sort out the binary operators, taking care to respect operator precedence.
        enum ExpressionPart {
            Atomic(Expression),
            BinaryOperator(BinaryOperator),
        }
        let mut parts = Vec::new();
        loop {
            match tokenizer.peek() {
                token if token == close.as_ref() => {
                    break;
                }
                Some(Token::BinaryOperator(binary_operator)) => {
                    match parts.last() {
                        Some(ExpressionPart::Atomic(_)) => {}
                        _ => panic!("Expected atomic expression, got {:?}", binary_operator),
                    }
                    parts.push(ExpressionPart::BinaryOperator(*binary_operator));
                    tokenizer.next();
                }
                Some(Token::OpenParen) => {
                    tokenizer.next();
                    let expression = self.parse_expression(tokenizer, Some(Token::CloseParen));
                    parts.push(ExpressionPart::Atomic(expression));
                    assert_eq!(tokenizer.next(), Some(Token::CloseParen), "Expected ')'");
                }
                _ => {
                    match parts.last() {
                        Some(ExpressionPart::Atomic(_)) => {
                            panic!("Expected binary operator, got {:?}", tokenizer.peek())
                        }
                        _ => {}
                    }
                    let atomic_expression = self.parse_atomic_expression(tokenizer);
                    parts.push(ExpressionPart::Atomic(atomic_expression));
                }
            }
        }
        // Operator precedence: Implies/EquivalentTo > And > Or
        // First we handle implies and equivalent to.
        let mut i = 0;
        while i < parts.len() {
            match parts[i] {
                ExpressionPart::BinaryOperator(binary_operator)
                    if binary_operator == BinaryOperator::Implies
                        || binary_operator == BinaryOperator::EquivalentTo =>
                {
                    let left = match parts.get(i - 1) {
                        Some(ExpressionPart::Atomic(expression)) => expression.clone(),
                        _ => unreachable!(),
                    };
                    let right = match parts.get(i + 1) {
                        Some(ExpressionPart::Atomic(expression)) => expression.clone(),
                        _ => unreachable!(),
                    };
                    parts.drain(i - 1..=i + 1);
                    parts.insert(
                        i - 1,
                        ExpressionPart::Atomic(Expression::BinaryOperator(
                            binary_operator,
                            Box::new(left),
                            Box::new(right),
                        )),
                    );
                }
                _ => i += 1,
            }
        }
        // Next we handle and.
        let mut i = 0;
        while i < parts.len() {
            match parts[i] {
                ExpressionPart::BinaryOperator(BinaryOperator::And) => {
                    let left = match parts.get(i - 1) {
                        Some(ExpressionPart::Atomic(expression)) => expression.clone(),
                        _ => unreachable!(),
                    };
                    let right = match parts.get(i + 1) {
                        Some(ExpressionPart::Atomic(expression)) => expression.clone(),
                        _ => unreachable!(),
                    };
                    parts.drain(i - 1..=i + 1);
                    parts.insert(
                        i - 1,
                        ExpressionPart::Atomic(Expression::BinaryOperator(
                            BinaryOperator::And,
                            Box::new(left),
                            Box::new(right),
                        )),
                    );
                }
                _ => i += 1,
            }
        }
        // Finally we handle or.
        let mut i = 0;
        while i < parts.len() {
            match parts[i] {
                ExpressionPart::BinaryOperator(BinaryOperator::Or) => {
                    let left = match parts.get(i - 1) {
                        Some(ExpressionPart::Atomic(expression)) => expression.clone(),
                        _ => unreachable!(),
                    };
                    let right = match parts.get(i + 1) {
                        Some(ExpressionPart::Atomic(expression)) => expression.clone(),
                        _ => unreachable!(),
                    };
                    parts.drain(i - 1..=i + 1);
                    parts.insert(
                        i - 1,
                        ExpressionPart::Atomic(Expression::BinaryOperator(
                            BinaryOperator::Or,
                            Box::new(left),
                            Box::new(right),
                        )),
                    );
                }
                _ => i += 1,
            }
        }
        match parts.get(0) {
            Some(ExpressionPart::Atomic(expression)) => expression.clone(),
            _ => unreachable!(),
        }
    }

    fn parse_atomic_expression(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        // Atomic expressions are quantifier chains, negations and predicates.
        match tokenizer.peek() {
            Some(Token::Quantifier(_)) => self.parse_quantifier_chain(tokenizer),
            Some(Token::Negation) => {
                tokenizer.next();
                let expression = self.parse_atomic_expression(tokenizer);
                Expression::Negation(Box::new(expression))
            }
            Some(Token::Identifier(_)) => self.parse_predicate(tokenizer),
            _ => panic!("Unexpected token: {:?}", tokenizer.peek()),
        }
    }

    fn parse_quantifier_chain(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        // Quantifier chains are some number of `for all` or `there exists` quantifiers followed by an expression in braces.
        let mut quantifiers = Vec::new();
        loop {
            match tokenizer.next() {
                Some(Token::Quantifier(quantifier)) => {
                    match tokenizer.peek() {
                        Some(Token::Identifier(identifier)) => {
                            quantifiers.push((quantifier.clone(), identifier.clone()));
                        }
                        _ => panic!("Expected identifier, got {:?}", tokenizer.peek()),
                    }
                    tokenizer.next();
                }
                Some(Token::OpenBrace) => {
                    break;
                }
                _ => panic!(
                    "Expected quantifier or open brace, got {:?}",
                    tokenizer.peek()
                ),
            }
        }
        assert!(!quantifiers.is_empty(), "Expected at least one quantifier");
        let expression = self.parse_expression(tokenizer, Some(Token::CloseBrace));
        assert_eq!(tokenizer.next(), Some(Token::CloseBrace), "Expected '}}'");
        Expression::QuantifierChain(quantifiers, Box::new(expression))
    }

    fn parse_predicate(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        // There are three forms of predicate: named predicates, function-like predicates and infix predicates.
        // They all start with an identifier.
        let first_identifier = match tokenizer.next() {
            Some(Token::Identifier(identifier)) => identifier,
            _ => panic!("Expected identifier, got {:?}", tokenizer.peek()),
        };
        // function-like predicates now have a '('. infix predicates have another identifier. Anything else is a named predicate.
        match tokenizer.peek().cloned() {
            Some(Token::OpenParen) => {
                tokenizer.next();
                let mut arguments = Vec::new();
                loop {
                    match tokenizer.peek() {
                        Some(Token::CloseParen) => {
                            break;
                        }
                        Some(Token::Identifier(argument)) => {
                            arguments.push(argument.clone());
                            tokenizer.next();
                            match tokenizer.peek() {
                                Some(Token::Comma) => {
                                    tokenizer.next();
                                }
                                Some(Token::CloseParen) => {}
                                _ => {
                                    panic!("Expected ',' or ')', got {:?}", tokenizer.peek());
                                }
                            }
                        }
                        _ => {
                            panic!("Expected identifier or ')', got {:?}", tokenizer.peek());
                        }
                    }
                }
                assert_eq!(tokenizer.next(), Some(Token::CloseParen), "Expected ')'");
                Expression::Predicate(first_identifier, arguments)
            }
            Some(Token::Identifier(second_identifier)) => {
                tokenizer.next();
                // The second identifier is the name of the predicate. The third one is the second argument (all infix predicates are binary).
                let third_identifier = match tokenizer.next() {
                    Some(Token::Identifier(identifier)) => identifier,
                    _ => panic!("Expected identifier, got {:?}", tokenizer.peek()),
                };
                Expression::Predicate(
                    second_identifier.clone(),
                    vec![first_identifier, third_identifier],
                )
            }
            _ => Expression::Predicate(first_identifier, Vec::new()),
        }
    }
}
