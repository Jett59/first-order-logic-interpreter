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
    PredicateIdentifier(String),
    FunctionIdentifier(String),
    ObjectIdentifier(String),
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
            '~' => {
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
            // ':' introduces a function identifier, '?' introduces a predicate identifier and otherwise it is assumed to be an object identifier.
            ':' => {
                let mut identifier = String::new();
                for c in self.remaining_text.chars().skip(1) {
                    if c.is_alphanumeric() || c == '_' {
                        identifier.push(c);
                    } else {
                        break;
                    }
                }
                if identifier.len() > 0 {
                    self.remaining_text = self.remaining_text[identifier.len() + 1..].to_string();
                    Some(Token::FunctionIdentifier(identifier))
                } else {
                    panic!("Unexpected character: {:?}", first_char);
                }
            }
            // The symbols +, -, *, /, %, ^, & and | are also function identifiers.
            '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' => {
                let identifier = self.remaining_text.chars().next().unwrap().to_string();
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::FunctionIdentifier(identifier))
            }
            '?' => {
                let mut identifier = String::new();
                for c in self.remaining_text.chars().skip(1) {
                    if c.is_alphanumeric() || c == '_' {
                        identifier.push(c);
                    } else {
                        break;
                    }
                }
                if identifier.len() > 0 {
                    self.remaining_text = self.remaining_text[identifier.len() + 1..].to_string();
                    Some(Token::PredicateIdentifier(identifier))
                } else {
                    panic!("Unexpected character: {:?}", first_char);
                }
            }
            // The symbols =, <, >, <=, >=, != and IN are also predicate identifiers.
            '!' | '<' | '>' if self.remaining_text.chars().nth(1) == Some('=') => {
                self.remaining_text = self.remaining_text["X=".len()..].to_string();
                Some(Token::PredicateIdentifier(
                    self.remaining_text[..=1].to_string(),
                ))
            }
            '=' | '<' | '>' => {
                let identifier = self.remaining_text.chars().next().unwrap().to_string();
                self.remaining_text = self.remaining_text[1..].to_string();
                Some(Token::PredicateIdentifier(identifier))
            }
            'I' if self.remaining_text.starts_with("IN") => {
                self.remaining_text = self.remaining_text["IN".len()..].to_string();
                Some(Token::PredicateIdentifier("IN".to_string()))
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
                    Some(Token::ObjectIdentifier(identifier))
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
            Some(Token::FunctionIdentifier(_))
            | Some(Token::PredicateIdentifier(_))
            | Some(Token::ObjectIdentifier(_)) => self.parse_predicate(tokenizer),
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
                        Some(Token::ObjectIdentifier(identifier)) => {
                            quantifiers.push((quantifier.clone(), identifier.clone()));
                        }
                        _ => panic!("Expected object identifier, got {:?}", tokenizer.peek()),
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
        // Predicates can be quite complex, so we need to handle a lot of different cases.
        // Function calling ones are fairly straight forward; we just have to recurse to parse the arguments.
        // Infix ones are more complex though due to syntactic ambiguities. For simplicity we'll require infix function calls to be surrounded by parentheses.
        // Since they can't be chained, infix predicates don't need parentheses around them.
        // The main rule is that there must be a predicate at the base level, possibly with functions and what not in the middle.
        // Predicates may not appear in the middle of a function call or as arguments to another predicate. That is the job of logical operators.

        let next_token = tokenizer.peek();
        if let Some(Token::PredicateIdentifier(_)) = next_token {
            // It is a predicate function
            return self.parse_predicate_call(tokenizer);
        }

        // Otherwise it is infix
        let mut arguments = Vec::new();
        arguments.push(self.parse_predicate_argument(tokenizer));
        let next_token = tokenizer.peek();
        let operator = match next_token {
            Some(Token::PredicateIdentifier(identifier)) => identifier.clone(),
            _ => panic!("Expected predicate identifier, got {:?}", next_token),
        };
        tokenizer.next();
        arguments.push(self.parse_predicate_argument(tokenizer));
        Expression::Predicate(operator, arguments)
    }

    fn parse_predicate_argument(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        let next_token = tokenizer.peek();
        if let Some(Token::FunctionIdentifier(_)) = next_token {
            self.parse_function_call(tokenizer)
        } else if let Some(Token::ObjectIdentifier(identifier)) = next_token {
            let identifier = identifier.clone();
            tokenizer.next();
            Expression::Object(identifier)
        } else if let Some(Token::OpenParen) = next_token {
            tokenizer.next();
            let result = self.parse_function_expression(tokenizer);
            assert_eq!(tokenizer.next(), Some(Token::CloseParen), "Expected ')'");
            result
        } else {
            panic!(
                "Expected function or object identifier, got {:?}",
                next_token
            );
        }
    }

    fn parse_predicate_call(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        let predicate = match tokenizer.next() {
            Some(Token::PredicateIdentifier(identifier)) => identifier,
            _ => panic!("Expected predicate identifier, got {:?}", tokenizer.peek()),
        };
        match tokenizer.peek() {
            Some(Token::OpenParen) => {}
            _ => return Expression::Predicate(predicate, Vec::new()), // Just a named predicate
        }
        tokenizer.next();
        let mut arguments = Vec::new();
        loop {
            if tokenizer.peek() == Some(&Token::CloseParen) {
                tokenizer.next();
                break;
            }
            arguments.push(self.parse_predicate_argument(tokenizer));
            match tokenizer.peek() {
                Some(Token::Comma) => {
                    tokenizer.next();
                }
                Some(Token::CloseParen) => {
                    tokenizer.next();
                    break;
                }
                _ => panic!("Expected ',' or ')', got {:?}", tokenizer.peek()),
            }
        }
        Expression::Predicate(predicate, arguments)
    }

    fn parse_function_call(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        let function = match tokenizer.next() {
            Some(Token::FunctionIdentifier(identifier)) => identifier,
            _ => panic!("Expected function identifier, got {:?}", tokenizer.peek()),
        };
        assert_eq!(tokenizer.next(), Some(Token::OpenParen), "Expected '('");
        let mut arguments = Vec::new();
        loop {
            let next_token = tokenizer.peek();
            if next_token == Some(&Token::CloseParen) {
                break;
            }
            arguments.push(self.parse_function_expression(tokenizer));
            match tokenizer.peek() {
                Some(Token::Comma) => {
                    tokenizer.next();
                }
                Some(Token::CloseParen) => {
                    tokenizer.next();
                    break;
                }
                _ => panic!("Expected ',' or ')', got {:?}", tokenizer.peek()),
            }
        }
        Expression::Function(function, arguments)
    }

    fn parse_function_expression(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        // This is either an object, a function call or an infix function call.
        let first_component = self.parse_function_argument(tokenizer);
        let next_token = tokenizer.peek();
        // If it is a function identifier, it is an infix function call.
        // Otherwise we don't care.
        if let Some(Token::FunctionIdentifier(_)) = next_token {
            let next_token = tokenizer.next();
            let operator = match next_token {
                Some(Token::FunctionIdentifier(identifier)) => identifier,
                _ => panic!("Expected function identifier, got {:?}", next_token),
            };
            let second_component = self.parse_function_argument(tokenizer);
            Expression::Function(operator, vec![first_component, second_component])
        } else {
            first_component
        }
    }

    fn parse_function_argument(&self, tokenizer: &mut Peekable<Tokenizer>) -> Expression {
        let next_token = tokenizer.peek();
        match next_token {
            Some(Token::FunctionIdentifier(_)) => self.parse_function_call(tokenizer),
            Some(Token::ObjectIdentifier(identifier)) => {
                let identifier = identifier.clone();
                tokenizer.next();
                Expression::Object(identifier)
            }
            Some(Token::OpenParen) => {
                tokenizer.next();
                let result = self.parse_function_expression(tokenizer);
                assert_eq!(tokenizer.next(), Some(Token::CloseParen), "Expected ')'");
                result
            }
            _ => panic!(
                "Expected function or object identifier, got {:?}",
                next_token
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let parser = Parser::new();
        let expression = parser.parse("(?P(x) && ?Q(x)) -> ?R(x)".to_string());
        assert_eq!(
            expression,
            Expression::BinaryOperator(
                BinaryOperator::Implies,
                Box::new(Expression::BinaryOperator(
                    BinaryOperator::And,
                    Box::new(Expression::Predicate(
                        "P".to_string(),
                        vec![Expression::Object("x".to_string())]
                    )),
                    Box::new(Expression::Predicate(
                        "Q".to_string(),
                        vec![Expression::Object("x".to_string())]
                    ))
                )),
                Box::new(Expression::Predicate(
                    "R".to_string(),
                    vec![Expression::Object("x".to_string())]
                ))
            )
        );
    }

    #[test]
    fn test_parse_quantifier_chain() {
        let parser = Parser::new();
        let expression = parser.parse("for all x there exists z {?P(x, z)}".to_string());
        assert_eq!(
            expression,
            Expression::QuantifierChain(
                vec![
                    (Quantifier::ForAll, "x".to_string()),
                    (Quantifier::ThereExists, "z".to_string())
                ],
                Box::new(Expression::Predicate(
                    "P".to_string(),
                    vec![
                        Expression::Object("x".to_string()),
                        Expression::Object("z".to_string())
                    ]
                ))
            )
        );
    }

    #[test]
    fn test_parse_operator_precedence() {
        let parser = Parser::new();
        let expression = parser.parse("?P(x) && ?Q(x) || ?R(x) -> ?S(x) <-> ?T(x)".to_string());
        assert_eq!(
            expression,
            Expression::BinaryOperator(
                BinaryOperator::Or,
                Box::new(Expression::BinaryOperator(
                    BinaryOperator::And,
                    Box::new(Expression::Predicate(
                        "P".to_string(),
                        vec![Expression::Object("x".to_string())]
                    )),
                    Box::new(Expression::Predicate(
                        "Q".to_string(),
                        vec![Expression::Object("x".to_string())]
                    ))
                )),
                Box::new(Expression::BinaryOperator(
                    BinaryOperator::EquivalentTo,
                    Box::new(Expression::BinaryOperator(
                        BinaryOperator::Implies,
                        Box::new(Expression::Predicate(
                            "R".to_string(),
                            vec![Expression::Object("x".to_string())]
                        )),
                        Box::new(Expression::Predicate(
                            "S".to_string(),
                            vec![Expression::Object("x".to_string())]
                        ))
                    )),
                    Box::new(Expression::Predicate(
                        "T".to_string(),
                        vec![Expression::Object("x".to_string())]
                    )),
                ))
            )
        );
    }

    #[test]
    fn test_negated_predicates() {
        let parser = Parser::new();
        let expression = parser.parse("~?P && ~:Q(x) ?SUBSET z && x IN y".to_string());
        assert_eq!(
            expression,
            Expression::BinaryOperator(
                BinaryOperator::And,
                Box::new(Expression::BinaryOperator(
                    BinaryOperator::And,
                    Box::new(Expression::Negation(Box::new(Expression::Predicate(
                        "P".to_string(),
                        Vec::new()
                    )))),
                    Box::new(Expression::Negation(Box::new(Expression::Predicate(
                        "SUBSET".to_string(),
                        vec![
                            Expression::Function(
                                "Q".to_string(),
                                vec![Expression::Object("x".to_string())]
                            ),
                            Expression::Object("z".to_string())
                        ]
                    ))))
                )),
                Box::new(Expression::Predicate(
                    "IN".to_string(),
                    vec![
                        Expression::Object("x".to_string()),
                        Expression::Object("y".to_string())
                    ]
                ))
            )
        );
    }

    #[test]
    fn test_function_compositions() {
        let parser = Parser::new();
        let expression = parser.parse(":_(x+y)=7 -> :f(x, y+1) IN z".to_string());
        assert_eq!(
            expression,
            Expression::BinaryOperator(
                BinaryOperator::Implies,
                Box::new(Expression::Predicate(
                    "=".to_string(),
                    vec![
                        Expression::Function(
                            "_".to_string(),
                            vec![Expression::Function(
                                "+".to_string(),
                                vec![
                                    Expression::Object("x".to_string()),
                                    Expression::Object("y".to_string())
                                ]
                            )]
                        ),
                        Expression::Object("7".to_string())
                    ]
                )),
                Box::new(Expression::Predicate(
                    "IN".to_string(),
                    vec![
                        Expression::Function(
                            "f".to_string(),
                            vec![
                                Expression::Object("x".to_string()),
                                Expression::Function(
                                    "+".to_string(),
                                    vec![
                                        Expression::Object("y".to_string()),
                                        Expression::Object("1".to_string())
                                    ]
                                )
                            ]
                        ),
                        Expression::Object("z".to_string())
                    ]
                ))
            )
        );
    }
}
