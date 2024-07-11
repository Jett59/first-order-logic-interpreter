use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use bit_set::BitSet;

#[derive(Debug, Clone)]
pub enum LogicalOperatorName {
    Not,
    And,
    Or,
    Implies,
    Biconditional,
}

impl TryFrom<&str> for LogicalOperatorName {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "¬" => Ok(LogicalOperatorName::Not),
            "∧" => Ok(LogicalOperatorName::And),
            "∨" => Ok(LogicalOperatorName::Or),
            "→" => Ok(LogicalOperatorName::Implies),
            "↔" => Ok(LogicalOperatorName::Biconditional),
            _ => Err(format!("Invalid logical operator: {}", value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedFormula {
    Atomic(String, Vec<String>),
    Compound(LogicalOperatorName, Vec<ParsedFormula>),
    Universal(String, Box<ParsedFormula>),
    Existential(String, Box<ParsedFormula>),
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Formula {
    Atomic(usize, Box<[usize]>),
    TruthTable(TruthTable),
    Universal(usize, Box<Formula>),
}

/// A truth table for a set of formulae
///
/// The truth table is represented as a bit set, where each bit represents the truth value of the formulae for a given input
/// The input can be found by converting the index to binary, and using the bits as the truth values for the formulae
/// The formulae are sorted by their hash, and the results are stored in the same order
#[derive(Debug, Clone, Hash, PartialEq)]
pub struct TruthTable {
    formulae: Vec<Formula>,
    results: BitSet,
}

pub type LogicalOperator = dyn Fn(&[bool]) -> bool;

impl TruthTable {
    pub fn new(mut formulae: Vec<Formula>, relationship: &LogicalOperator) -> Self {
        // To allow equality comparisons to work, we need to sort the formulae by their hash
        // However, we also need to supply the original order of the formulae to the relationship, so we keep track of the original indices as well
        let mut indices: Vec<usize> = (0..formulae.len()).collect();
        indices.sort_by_key(|&i| calculate_hash(&formulae[i]));
        formulae.sort_by_key(|formula| calculate_hash(formula));

        // Calculate the results for the truth table
        let mut results = BitSet::new();
        for i in 0..(1 << formulae.len()) {
            let values = indices
                .iter()
                .map(|j| (i >> j) & 1 == 1)
                .collect::<Vec<_>>();
            let result = relationship(&values);
            if result {
                results.insert(i);
            }
        }
        Self { formulae, results }
    }

    pub fn as_logical_operator(&self) -> impl Fn(&[bool]) -> bool + '_ {
        move |values| {
            // Convert the boolean values to an index in the truth table
            let index = values
                .iter()
                .enumerate()
                .fold(0, |acc, (i, &value)| acc | ((value as usize) << i));
            self.results.contains(index)
        }
    }
}

fn calculate_hash<T: Hash>(value: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_truth_table_commutative() {
        let formulae = vec![
            Formula::Atomic(0, Box::new([1])),
            Formula::Atomic(1, Box::new([0])),
        ];
        let relationship = |values: &[bool]| values[0] && values[1];
        let truth_table = TruthTable::new(formulae, &relationship);
        let logical_operator = truth_table.as_logical_operator();
        assert_eq!(logical_operator(&[false, false]), false);
        assert_eq!(logical_operator(&[false, true]), false);
        assert_eq!(logical_operator(&[true, false]), false);
        assert_eq!(logical_operator(&[true, true]), true);
    }

    #[test]
    fn test_truth_table_complex() {
        // Since the order may be changed, we should compare equivalent truth tables instead of checking for an exact match
        let formulae = vec![
            Formula::Atomic(0, Box::new([1])),
            Formula::Atomic(1, Box::new([0])),
        ];
        let relationship1 = |values: &[bool]| !values[0] || values[1];
        let truth_table1 = TruthTable::new(formulae.clone(), &relationship1);
        let relationship2 = |values: &[bool]| !(values[0] && !values[1]);
        let truth_table2 = TruthTable::new(formulae.clone(), &relationship2);
        assert_eq!(truth_table1, truth_table2);
        let relationship3 = |values: &[bool]| values[0] && !values[1];
        let truth_table3 = TruthTable::new(formulae, &relationship3);
        assert_ne!(truth_table1, truth_table3);

        // The first two could be either of these:
        let results_candidate1 =  BitSet::from_bytes(&[0b10110000]);
        let results_candidate2 = BitSet::from_bytes(&[0b11010000]);
        
        assert!(
            results_candidate1 == truth_table1.results
                || results_candidate2 == truth_table1.results
        );
    }

    #[test]
    fn test_truth_table_out_of_order() {
        let formulas1 = vec![
            Formula::Atomic(0, Box::new([1])),
            Formula::Atomic(1, Box::new([0])),
        ];
        let formulas2 = vec![
            Formula::Atomic(1, Box::new([0])),
            Formula::Atomic(0, Box::new([1])),
        ];
        let relationship1 = |values: &[bool]| !values[0] || values[1];
        let relationship2 = |values: &[bool]| !values[1] || values[0];
        let truth_table1 = TruthTable::new(formulas1, &relationship1);
        let truth_table2 = TruthTable::new(formulas2, &relationship2);
        assert_eq!(truth_table1, truth_table2);
    }
}
