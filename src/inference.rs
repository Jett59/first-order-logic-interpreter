use std::collections::HashMap;

use crate::{BinaryOperator, Expression, Quantifier};

fn uses_variable(expression: &Expression, variable: &str) -> bool {
    match expression {
        Expression::BinaryOperator(_, left, right) => {
            uses_variable(left, variable) && uses_variable(right, variable)
        }
        Expression::Negation(inner) => uses_variable(inner, variable),
        Expression::QuantifierChain(quantifiers, inner) => {
            !quantifiers
                .iter()
                .any(|(_, quantified_variable)| quantified_variable == variable)
                && uses_variable(inner, variable)
        }
        Expression::Predicate(_, arguments) => arguments
            .iter()
            .any(|argument| uses_variable(argument, variable)),
        Expression::Function(_, arguments) => arguments
            .iter()
            .any(|argument| uses_variable(argument, variable)),
        Expression::Object(object) => object == variable,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Formula {
    expression: Expression,
    free_variables: Vec<String>,
}

pub fn generate_implication_table(statement: &Expression) -> HashMap<Formula, Formula> {
    // Essentially, we look for any 'implies' or 'equivalent to' expressions.
    // The free variables are only those which have a universal quantifier; the rest of the variables have to be bound within the formula.
    // Additionally, we need to make sure that the implication is within a 'always true' context (and/nested implication).
    // For 'not's in a n always true context, we can just negate the formula.
    let mut implication_table = HashMap::new();
    match statement {
        Expression::BinaryOperator(operator, left, right) => match operator {
            BinaryOperator::Implies => {
                let left_formula = Formula {
                    expression: (**left).clone(),
                    free_variables: vec![],
                };
                let right_formula = Formula {
                    expression: (**right).clone(),
                    free_variables: vec![],
                };
                implication_table.insert(left_formula, right_formula);
            }
            BinaryOperator::EquivalentTo => {
                let left_formula = Formula {
                    expression: (**left).clone(),
                    free_variables: vec![],
                };
                let right_formula = Formula {
                    expression: (**right).clone(),
                    free_variables: vec![],
                };
                implication_table.insert(left_formula.clone(), right_formula.clone());
                implication_table.insert(right_formula, left_formula);
            }
            BinaryOperator::And => {
                implication_table.extend(generate_implication_table(left));
                implication_table.extend(generate_implication_table(right));
            }
            BinaryOperator::Or => {}
        },
        Expression::Negation(inner) => {
            let inner_implications = generate_implication_table(inner);
            for (key, value) in inner_implications {
                implication_table.insert(
                    key,
                    Formula {
                        expression: Expression::Negation(Box::new(value.expression)),
                        free_variables: value.free_variables,
                    },
                );
            }
        }
        Expression::QuantifierChain(quantifiers, inner) => {
            let inner_implications = generate_implication_table(inner);
            for (key, value) in inner_implications {
                // Remember: only 'for all' quantifiers are considered free variables.
            // The 'there exists' parts have to be inserted into every implication of the body. So do 'for all' quantifiers which aren't referenced in the left-hand side.
                            let free_variables: Vec<String> = quantifiers
                .iter()
                .filter_map(|quantifier| match quantifier {
                    (Quantifier::ForAll, variable) if uses_variable(inner, variable) => Some(variable.clone()),
                    _ => None,
                })
                .collect();
            let projected_variables: Vec<(Quantifier, String)> = quantifiers
                .iter()
                .filter(|(_, variable)|!free_variables.contains(variable))
                .cloned()
                .collect();
                let mut new_key = key.clone();
                new_key.free_variables.extend(free_variables.clone());
                let new_expression = if projected_variables.is_empty() {
                    value.expression
                } else {
                    Expression::QuantifierChain(
                        projected_variables.clone(),
                        Box::new(value.expression),
                    )
                };
                let new_value = Formula {
                    expression: new_expression,
                    free_variables: value.free_variables,
                };
                implication_table.insert(new_key, new_value);
            }
        }
        Expression::Predicate(_, _) | Expression::Function(_, _) | Expression::Object(_) => {}
    }
    implication_table
}

//pub fn infer(axioms: Vec<Expression>, statement: Expression) -> Vec<Expression> {}
