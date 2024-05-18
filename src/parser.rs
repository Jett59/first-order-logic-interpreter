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
    End,
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

trait ParserTrait {
    /// Returns the next parser to use and whether the current token should be passed in again.
    ///
    /// In general, the token should be passed again if a new parser was created and the token
    /// is important for whatever the new parser is parsing.
    fn receive_token(self: Box<Self>, token: Token) -> (Box<dyn ParserTrait>, bool);
    fn receive_subexpression(self: Box<Self>, subexpression: Expression) -> Box<dyn ParserTrait>;

    fn expression(&self) -> Option<Expression> {
        None
    }
}

pub struct Parser {
    state: Option<Box<dyn ParserTrait>>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            state: Some(Box::new(ExpressionParser::default())),
        }
    }

    pub fn parse(&self, text: String) -> Expression {
        let mut parser: Box<dyn ParserTrait> = Box::new(ExpressionParser::default());
        for token in Tokenizer::new(text) {
            let mut receiving_this_token = true;
            while receiving_this_token {
                let (new_parser, keep_receiving_token) = parser.receive_token(token.clone());
                parser = new_parser;
                receiving_this_token = keep_receiving_token;
            }
        }
        let mut keep_receiving_end = true;
        while keep_receiving_end {
            let (new_parser, keep_receiving_token) = parser.receive_token(Token::End);
            parser = new_parser;
            keep_receiving_end = keep_receiving_token;
        }
        parser.expression().expect("Unexpected end of input")
    }
}

#[derive(Default)]
struct ExpressionParser {
    previous_parser: Option<Box<dyn ParserTrait>>,
    expression: Option<Expression>,
}

impl ExpressionParser {
    fn new(previous: Box<dyn ParserTrait>) -> ExpressionParser {
        ExpressionParser {
            previous_parser: Some(previous),
            expression: None,
        }
    }
}

impl ParserTrait for ExpressionParser {
    fn receive_token(self: Box<Self>, token: Token) -> (Box<dyn ParserTrait>, bool) {
        match token {
            Token::Quantifier(_) => (Box::new(QuantifierParser::new(self)), true),
            Token::Negation => (Box::new(NegationParser::new(self)), true),
            Token::OpenParen => (Box::new(ExpressionParser::new(self)), false),
            Token::Identifier(_) => (Box::new(PredicateParser::new(self)), true),
            Token::CloseParen | Token::CloseBrace | Token::End => {
                if let Some(previous_parser) = self.previous_parser {
                    (
                        previous_parser.receive_subexpression(self.expression.unwrap()),
                        token == Token::End, // Keep receiving the 'end' token.
                    )
                } else {
                    // We are the root parser, so terminate the chain here.
                    (self, false)
                }
            }
            _ => panic!("Unexpected token: {:?}", token),
        }
    }

    fn receive_subexpression(
        mut self: Box<Self>,
        subexpression: Expression,
    ) -> Box<dyn ParserTrait> {
        self.expression = Some(subexpression);
        self
    }

    fn expression(&self) -> Option<Expression> {
        Some(self.expression.clone().unwrap())
    }
}

struct QuantifierParser {
    previous_parser: Box<dyn ParserTrait>,
    next_quantifier: Option<Quantifier>,
    quantifiers: Vec<(Quantifier, String)>,
}

impl QuantifierParser {
    fn new(previous: Box<dyn ParserTrait>) -> QuantifierParser {
        QuantifierParser {
            previous_parser: previous,
            next_quantifier: None,
            quantifiers: Vec::new(),
        }
    }
}

impl ParserTrait for QuantifierParser {
    fn receive_token(mut self: Box<Self>, token: Token) -> (Box<dyn ParserTrait>, bool) {
        match token {
            Token::Quantifier(quantifier) => {
                assert!(
                    self.next_quantifier.is_none(),
                    "Unexpected token: {:?} (expecting identifier)",
                    token
                );
                self.next_quantifier = Some(quantifier);
                (self, false)
            }
            Token::Identifier(ref identifier) => {
                assert!(
                    self.next_quantifier.is_some(),
                    "Unexpected token: {:?} (expecting quantifier or `{{`)",
                    token
                );
                self.quantifiers
                    .push((self.next_quantifier.unwrap(), identifier.clone()));
                self.next_quantifier = None;
                (self, false)
            }
            Token::OpenBrace => (Box::new(ExpressionParser::new(self)), false),
            _ => panic!("Unexpected token: {:?}", token),
        }
    }

    fn receive_subexpression(
        mut self: Box<Self>,
        subexpression: Expression,
    ) -> Box<dyn ParserTrait> {
        let expression = Expression::QuantifierChain(self.quantifiers, Box::new(subexpression));
        self.previous_parser.receive_subexpression(expression)
    }
}

struct NegationParser {
    previous_parser: Box<dyn ParserTrait>,
    found_negation: bool,
}

impl NegationParser {
    fn new(previous: Box<dyn ParserTrait>) -> NegationParser {
        NegationParser {
            previous_parser: previous,
            found_negation: false,
        }
    }
}

impl ParserTrait for NegationParser {
    fn receive_token(mut self: Box<Self>, token: Token) -> (Box<dyn ParserTrait>, bool) {
        match self.found_negation {
            false => {
                assert_eq!(
                    token,
                    Token::Negation,
                    "Unexpected token: {:?} (expecting `!`)",
                    token
                );
                self.found_negation = true;
                (self, false)
            }
            true => (Box::new(ExpressionParser::new(self)), true),
        }
    }

    fn receive_subexpression(self: Box<Self>, subexpression: Expression) -> Box<dyn ParserTrait> {
        let expression = Expression::Negation(Box::new(subexpression));
        self.previous_parser.receive_subexpression(expression)
    }
}

struct PredicateParser {
    previous_parser: Box<dyn ParserTrait>,
    first_identifier: Option<String>,
    is_function_call_like: Option<bool>,
    predicate_name: Option<String>,
    arguments: Vec<String>,
}

impl PredicateParser {
    fn new(previous: Box<dyn ParserTrait>) -> PredicateParser {
        PredicateParser {
            previous_parser: previous,
            first_identifier: None,
            is_function_call_like: None,
            predicate_name: None,
            arguments: Vec::new(),
        }
    }
}

impl ParserTrait for PredicateParser {
    fn receive_token(mut self: Box<Self>, token: Token) -> (Box<dyn ParserTrait>, bool) {
        match token {
            Token::Identifier(identifier) => match self.first_identifier {
                None => {
                    self.first_identifier = Some(identifier);
                    (self, false)
                }
                Some(ref first_token) if self.is_function_call_like != Some(true) => {
                    // These are predicates of the form `a R b` where `R` is a user-defined binary operator.
                    if self.predicate_name.is_none() {
                        self.predicate_name = Some(identifier);
                        self.arguments.push(first_token.clone());
                        self.is_function_call_like = Some(false);
                        (self, false)
                    } else {
                        self.arguments.push(identifier);
                        (
                            self.previous_parser
                                .receive_subexpression(Expression::Predicate(
                                    self.predicate_name.unwrap(),
                                    self.arguments,
                                )),
                            false,
                        )
                    }
                }
                Some(_) => {
                    self.arguments.push(identifier);
                    (self, false)
                }
            },
            Token::OpenParen => {
                assert!(
                    self.first_identifier.is_some(),
                    "Unexpected token: {:?} (expecting identifier or `(`)",
                    token
                );
                self.is_function_call_like = Some(true);
                self.predicate_name = self.first_identifier.clone();
                (self, false)
            }
            Token::CloseParen => {
                assert!(
                    self.is_function_call_like.is_some(),
                    "Unexpected token: {:?} (expecting identifier)",
                    token
                );
                let expression =
                    Expression::Predicate(self.first_identifier.unwrap(), self.arguments);
                (
                    self.previous_parser.receive_subexpression(expression),
                    false,
                )
            }
            _ => panic!("Unexpected token: {:?}", token),
        }
    }

    fn receive_subexpression(self: Box<Self>, _subexpression: Expression) -> Box<dyn ParserTrait> {
        unreachable!()
    }
}
