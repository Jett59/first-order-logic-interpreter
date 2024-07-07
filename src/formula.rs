#[derive(Debug, Clone)]
pub enum LogicalOperator {
    Not,
    And,
    Or,
    Implies,
    Biconditional,
}

impl TryFrom<&str> for LogicalOperator {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "¬" => Ok(LogicalOperator::Not),
            "∧" => Ok(LogicalOperator::And),
            "∨" => Ok(LogicalOperator::Or),
            "→" => Ok(LogicalOperator::Implies),
            "↔" => Ok(LogicalOperator::Biconditional),
            _ => Err(format!("Invalid logical operator: {}", value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Formula {
    Atomic(String, Vec<String>),
    Compound(LogicalOperator, Vec<Formula>),
    Universal(String, Box<Formula>),
    Existential(String, Box<Formula>),
}
