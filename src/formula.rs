use std::{
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone)]
pub enum NamedLogicalOperator {
    Not,
    And,
    Or,
    Implies,
    Biconditional,
}

impl TryFrom<&str> for NamedLogicalOperator {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "¬" => Ok(NamedLogicalOperator::Not),
            "∧" => Ok(NamedLogicalOperator::And),
            "∨" => Ok(NamedLogicalOperator::Or),
            "→" => Ok(NamedLogicalOperator::Implies),
            "↔" => Ok(NamedLogicalOperator::Biconditional),
            _ => Err(format!("Invalid logical operator: {}", value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedFormula {
    Atomic(String, Vec<String>),
    Compound(NamedLogicalOperator, Vec<ParsedFormula>),
    Universal(String, Box<ParsedFormula>),
    Existential(String, Box<ParsedFormula>),
}

pub trait LogicalOperator: Debug {
    fn evaluate(&self, operands: &[bool]) -> bool;
}

impl<T: LogicalOperator> LogicalOperator for &T
where
    T: ?Sized,
{
    fn evaluate(&self, operands: &[bool]) -> bool {
        (*self).evaluate(operands)
    }
}

impl LogicalOperator for NamedLogicalOperator {
    fn evaluate(&self, operands: &[bool]) -> bool {
        if operands.len() == 1 {
            match self {
                NamedLogicalOperator::Not => !operands[0],
                _ => panic!("Expected 2 operands, found 1 for {:?}", self),
            }
        } else if operands.len() == 2 {
            match self {
                NamedLogicalOperator::And => operands[0] && operands[1],
                NamedLogicalOperator::Or => operands[0] || operands[1],
                NamedLogicalOperator::Implies => !operands[0] || operands[1],
                NamedLogicalOperator::Biconditional => operands[0] == operands[1],
                _ => panic!("Expected 1 operand, found 2 for {:?}", self),
            }
        } else {
            panic!("Expected 1 or 2 operands, found {}", operands.len());
        }
    }
}

#[derive(Debug)]
pub struct ReorderedOperator<T: LogicalOperator> {
    operator: T,
    operands: Vec<usize>,
}

impl<T: LogicalOperator> ReorderedOperator<T> {
    pub fn new(operator: T, operands: Vec<usize>) -> Self {
        ReorderedOperator { operator, operands }
    }
}

impl<T: LogicalOperator> LogicalOperator for ReorderedOperator<T> {
    fn evaluate(&self, operands: &[bool]) -> bool {
        let mut reordered_operands = Vec::with_capacity(self.operands.len());
        for &index in &self.operands {
            reordered_operands.push(operands[index]);
        }
        self.operator.evaluate(&reordered_operands)
    }
}

#[derive(Clone, Debug)]
pub enum Formula {
    Atomic(usize, Vec<usize>),
    Compound(Rc<dyn LogicalOperator>, Vec<Formula>),
    Universal(usize, Box<Formula>),
}

fn allocate_variable() -> usize {
    static NEXT_VARIABLE: AtomicUsize = AtomicUsize::new(0);
    NEXT_VARIABLE.fetch_add(1, Ordering::Relaxed)
}

impl Formula {
    pub fn equivalent_to(&self, other: &Self) -> bool {
        use Formula::*;
        match (self, other) {
            (Atomic(my_predicate, my_operands), Atomic(other_predicate, other_operands)) => {
                my_predicate == other_predicate && my_operands == other_operands
            }
            (Compound(my_operator, my_operands), Compound(other_operator, other_operands)) => {
                if my_operands.len() != other_operands.len() {
                    return false;
                }
                // Reorderings of the operands are fine so long as the logical operators are equivalent after reordering
                let mut reordering = Vec::with_capacity(my_operands.len());
                for my_operand in my_operands {
                    let mut found = false;
                    for (i, other_operand) in other_operands.iter().enumerate() {
                        if !reordering.contains(&i) && my_operand.equivalent_to(other_operand) {
                            reordering.push(i);
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        return false;
                    }
                }
                // Check if the logical operators are equivalent after reordering
                let reordered_operator = ReorderedOperator::new(my_operator.as_ref(), reordering);
                // The only way to check if two logical operators are equivalent is to evaluate them on all possible inputs
                for i in 0..1 << my_operands.len() {
                    let operands = (0..my_operands.len())
                        .map(|j| i & (1 << j) != 0)
                        .collect::<Vec<_>>();
                    if reordered_operator.evaluate(&operands) != other_operator.evaluate(&operands)
                    {
                        return false;
                    }
                }
                true
            }
            (Universal(my_variable, my_formula), Universal(other_variable, other_formula)) => {
                my_formula
                    .rename_free_without_collisions(*my_variable, *other_variable)
                    .equivalent_to(other_formula)
            }
            _ => false,
        }
    }

    pub fn rename_free(&self, a: usize, b: usize) -> Self {
        use Formula::*;
        match self {
            Atomic(predicate, operands) => {
                let new_operands = operands
                    .iter()
                    .map(|operand| if *operand == a { b } else { *operand })
                    .collect::<Vec<_>>();
                Atomic(*predicate, new_operands)
            }
            Compound(operator, operands) => Compound(
                Rc::clone(operator),
                operands
                    .iter()
                    .map(|operand| operand.rename_free(a, b))
                    .collect(),
            ),
            Universal(variable, formula) if a != *variable => {
                let new_formula = formula.rename_free(a, b);
                Universal(*variable, Box::new(new_formula))
            }
            Universal(_, _) => self.clone(),
        }
    }

    pub fn rename_free_without_collisions(&self, a: usize, b: usize) -> Self {
        if a != b {
            self.rename_free(b, allocate_variable()).rename_free(a, b)
        } else {
            self.clone()
        }
    }
}

#[derive(Default, Clone)]
pub struct ParsedFormulaInterpreter {
    atomic_operators: HashMap<String, usize>,
    free_variables: HashMap<String, usize>,
}

fn allocate_atomic_operator_id() -> usize {
    static NEXT_ATOMIC_OPERATOR_ID: AtomicUsize = AtomicUsize::new(0);
    NEXT_ATOMIC_OPERATOR_ID.fetch_add(1, Ordering::Relaxed)
}

impl ParsedFormulaInterpreter {
    pub fn interpret(&mut self, parsed_formula: ParsedFormula) -> Formula {
        match parsed_formula {
            ParsedFormula::Atomic(operator, operands) => {
                if let Some(&id) = self.atomic_operators.get(&operator) {
                    Formula::Atomic(
                        id,
                        operands
                            .into_iter()
                            .map(|operand| {
                                *self
                                    .free_variables
                                    .entry(operand)
                                    .or_insert_with(allocate_variable)
                            })
                            .collect(),
                    )
                } else {
                    let id = allocate_atomic_operator_id();
                    self.atomic_operators.insert(operator, id);
                    Formula::Atomic(
                        id,
                        operands
                            .into_iter()
                            .map(|operand| {
                                *self
                                    .free_variables
                                    .entry(operand)
                                    .or_insert_with(allocate_variable)
                            })
                            .collect(),
                    )
                }
            }
            ParsedFormula::Compound(operator, operands) => Formula::Compound(
                Rc::new(operator),
                operands
                    .into_iter()
                    .map(|operand| self.interpret(operand))
                    .collect(),
            ),
            ParsedFormula::Universal(variable, inner) => {
                // Even though this is a new variable, it is still safe to use the same id as the old variable
                // If there would be collisions, it would also have been a collision in the original formula
                let variable_id = *self
                    .free_variables
                    .entry(variable)
                    .or_insert_with(allocate_variable);
                Formula::Universal(variable_id, Box::new(self.interpret(*inner)))
            }
            ParsedFormula::Existential(variable, inner) => {
                // To avoid duplication, just translate this to a universal formula
                self.interpret(ParsedFormula::Compound(
                    NamedLogicalOperator::Not,
                    vec![ParsedFormula::Universal(
                        variable,
                        Box::new(ParsedFormula::Compound(
                            NamedLogicalOperator::Not,
                            vec![*inner],
                        )),
                    )],
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use Formula::*;

    #[test]
    fn simple_equivalent_formulas() {
        let formula1 = Compound(
            Rc::new(NamedLogicalOperator::And),
            vec![Atomic(0, vec![0, 1]), Atomic(1, vec![1, 0])],
        );
        let formula2 = Compound(
            Rc::new(NamedLogicalOperator::And),
            vec![Atomic(1, vec![1, 0]), Atomic(0, vec![0, 1])],
        );
        assert!(formula1.equivalent_to(&formula2));

        let variable1 = allocate_variable();
        let variable2 = allocate_variable();
        let variable3 = allocate_variable();

        let formula1 = Universal(variable1, Box::new(Atomic(0, vec![variable1, variable3])));
        let formula2 = Universal(variable2, Box::new(Atomic(0, vec![variable2, variable3])));
        assert!(formula1.equivalent_to(&formula2));

        let formula1 = Universal(variable1, Box::new(Atomic(0, vec![variable1, variable2])));
        let formula2 = Universal(variable1, Box::new(Atomic(0, vec![variable1, variable2])));
        assert!(formula1.equivalent_to(&formula2));
    }

    #[test]
    fn simple_different_formulas() {
        let formula1 = Atomic(0, vec![0, 1]);
        let formula2 = Atomic(0, vec![1, 0]);
        assert!(!formula1.equivalent_to(&formula2));

        let variable1 = allocate_variable();
        let variable2 = allocate_variable();
        let variable3 = allocate_variable();

        let formula1 = Universal(variable1, Box::new(Atomic(0, vec![variable1, variable3])));
        let formula2 = Universal(variable3, Box::new(Atomic(0, vec![variable2, variable3])));
        assert!(!formula1.equivalent_to(&formula2));

        let formula1 = Compound(
            Rc::new(NamedLogicalOperator::Implies),
            vec![Atomic(0, vec![0, 1]), Atomic(1, vec![1, 0])],
        );
        let formula2 = Compound(
            Rc::new(NamedLogicalOperator::Implies),
            vec![Atomic(1, vec![1, 0]), Atomic(0, vec![0, 1])],
        );
        assert!(!formula1.equivalent_to(&formula2));

        let formula1 = Universal(
            variable1,
            Box::new(Universal(
                variable1,
                Box::new(Atomic(0, vec![variable1, variable3])),
            )),
        );
        let formula2 = Universal(
            variable2,
            Box::new(Universal(
                variable3,
                Box::new(Atomic(0, vec![variable2, variable3])),
            )),
        );
        assert!(!formula1.equivalent_to(&formula2));
    }
}
