use dashmap::DashSet;
use rayon::prelude::*;
use rustc_hash::FxHasher;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::gcd::gcd;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Eq)]
pub struct Operation {
    operator: Op,
    operands: (Number, Number),
    cached_hash: OnceLock<u64>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Number {
    pub value: u32,
    op: Option<Arc<Operation>>,
}

fn is_operator_similar(op: Op, op2: Op) -> bool {
    matches!(
        (op, op2),
        (Op::Add | Op::Sub, Op::Add | Op::Sub) | (Op::Mul | Op::Div, Op::Mul | Op::Div)
    )
}

impl PartialEq for Operation {
    fn eq(&self, other: &Self) -> bool {
        if !is_operator_similar(self.operator, other.operator) {
            return false;
        }
        if let (Some(h1), Some(h2)) = (self.cached_hash.get(), other.cached_hash.get())
            && h1 != h2
        {
            return false;
        }
        let (mut left_blocks, mut right_blocks) = get_building_blocks(self);
        left_blocks.sort_unstable_by_key(|n| n.value);
        right_blocks.sort_unstable_by_key(|n| n.value);

        let (mut left_blocks_other, mut right_blocks_other) = get_building_blocks(other);
        left_blocks_other.sort_unstable_by_key(|n| n.value);
        right_blocks_other.sort_unstable_by_key(|n| n.value);

        left_blocks == left_blocks_other && right_blocks == right_blocks_other
    }
}

pub fn get_building_blocks(op: &Operation) -> (Vec<&Number>, Vec<&Number>) {
    let (left, right) = &op.operands;

    // for mul and div left blocks are numerator, for add and sub they are positives
    let mut left_blocks = vec![];
    // for mul and div right blocks are denominator, for add and sub they are negatives
    let mut right_blocks = vec![];

    if let Some(left_op) = &left.op
        && is_operator_similar(left_op.operator, op.operator)
    {
        let (left_left_blocks, left_right_blocks) = get_building_blocks(left_op);
        left_blocks.extend(left_left_blocks);
        right_blocks.extend(left_right_blocks);
    } else {
        left_blocks.push(left);
    }
    if let Some(right_op) = &right.op
        && is_operator_similar(right_op.operator, op.operator)
    {
        let (right_left_blocks, right_right_blocks) = get_building_blocks(right_op);
        match &op.operator {
            Op::Mul | Op::Add => {
                left_blocks.extend(right_left_blocks);
                right_blocks.extend(right_right_blocks);
            }
            Op::Sub | Op::Div => {
                right_blocks.extend(right_left_blocks);
                left_blocks.extend(right_right_blocks);
            }
        }
    } else {
        match &op.operator {
            Op::Mul | Op::Add => {
                left_blocks.push(right);
            }
            Op::Sub | Op::Div => {
                right_blocks.push(right);
            }
        }
    }

    (left_blocks, right_blocks)
}

impl Operation {
    pub fn new(operator: Op, a: Number, b: Number) -> Self {
        Self {
            operator,
            operands: (a, b),
            cached_hash: OnceLock::new(),
        }
    }
}

impl Hash for Operation {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let hash = *self.cached_hash.get_or_init(|| {
            let mut hasher = FxHasher::default();

            let (mut left_blocks, mut right_blocks) = get_building_blocks(self);
            left_blocks.sort_unstable_by_key(|n| n.value);
            right_blocks.sort_unstable_by_key(|n| n.value);

            for num in left_blocks {
                num.hash(&mut hasher);
            }

            match self.operator {
                Op::Add | Op::Sub => Op::Add.hash(&mut hasher),
                Op::Mul | Op::Div => Op::Mul.hash(&mut hasher),
            }

            for num in right_blocks {
                num.hash(&mut hasher);
            }

            hasher.finish()
        });
        state.write_u64(hash);
    }
}

pub struct Scoreboard {
    pub best_score: AtomicU32,
    pub best_solutions: Mutex<DashSet<Number>>,
}

impl Scoreboard {
    fn new() -> Self {
        Self {
            best_score: AtomicU32::new(u32::MAX),
            best_solutions: Mutex::new(DashSet::default()),
        }
    }

    fn insert_if_better_or_same(&self, score: u32, num: Number) -> bool {
        let current_best = self.best_score.load(Ordering::Acquire);

        if score <= current_best {
            let solutions = self.best_solutions.lock().unwrap();
            let actual_best = self.best_score.load(Ordering::Relaxed);
            if score < actual_best {
                self.best_score.store(score, Ordering::Release);
                solutions.clear();
                return solutions.insert(num);
            } else if score == actual_best {
                return solutions.insert(num);
            }
        }
        false
    }

    pub fn simplify_and_deduplicate(&mut self) {
        let solutions = self.best_solutions.get_mut().unwrap();
        let simplified: DashSet<Number> = solutions.iter().map(|n| n.simplify()).collect();
        *solutions = simplified;
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operator_string = match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "x",
            Op::Div => "÷",
        };
        write!(f, "{}", operator_string)
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (num1, num2) = &self.operands;
        write!(f, "{} {} {}", num1.value, self.operator, num2.value)
    }
}

impl Debug for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::ops::Mul<Number> for Number {
    type Output = Operation;

    fn mul(self, rhs: Number) -> Self::Output {
        Operation::new(Op::Mul, self, rhs)
    }
}

impl std::ops::Div<Number> for Number {
    type Output = Operation;

    fn div(self, rhs: Number) -> Self::Output {
        Operation::new(Op::Div, self, rhs)
    }
}

impl std::ops::Add<Number> for Number {
    type Output = Operation;

    fn add(self, rhs: Number) -> Self::Output {
        Operation::new(Op::Add, self, rhs)
    }
}

impl std::ops::Sub<Number> for Number {
    type Output = Operation;

    fn sub(self, rhs: Number) -> Self::Output {
        Operation::new(Op::Sub, self, rhs)
    }
}

impl From<Operation> for Number {
    fn from(operation: Operation) -> Self {
        let left = &operation.operands.0;
        let right = &operation.operands.1;
        let value = match operation.operator {
            Op::Add => left.value + right.value,
            Op::Sub => left.value - right.value,
            Op::Div => left.value / right.value,
            Op::Mul => left.value * right.value,
        };
        Number {
            value,
            op: Some(Arc::new(operation)),
        }
    }
}

impl Number {
    pub fn new(value: u32) -> Self {
        Self { value, op: None }
    }

    pub fn simplify(&self) -> Self {
        let Some(op) = &self.op else {
            return self.clone();
        };

        let (blocks_l, blocks_r) = get_building_blocks(&op);

        let mut blocks_l: Vec<Number> = blocks_l.into_iter().map(|n| n.simplify()).collect();
        let mut blocks_r: Vec<Number> = blocks_r.into_iter().map(|n| n.simplify()).collect();
        blocks_r.sort_unstable_by_key(|n| n.value);

        // get rid of nonsensical operations like 3 * 2 / 2 = 3
        if blocks_l.len() > 1 {
            let mut counts: HashMap<&Number, usize> = HashMap::new();

            for n in &blocks_r {
                *counts.entry(n).or_insert(0) += 1;
            }

            blocks_l.retain(|n| {
                if let Some(c) = counts.get_mut(n) {
                    if *c > 0 {
                        *c -= 1;
                        return false;
                    }
                }
                true
            });

            blocks_r = counts
                .into_iter()
                .flat_map(|(value, count)| std::iter::repeat(value.clone()).take(count))
                .collect();
        }

        match op.operator {
            Op::Add | Op::Sub => {
                let left_sum = blocks_l
                    .into_iter()
                    .reduce(|acc, n| Number::from(acc + n))
                    .expect("Left blocks empty in Add");
                blocks_r
                    .into_iter()
                    .fold(left_sum, |acc, n| Number::from(acc - n))
            }
            Op::Mul | Op::Div => {
                let mut result: Option<Number> = None;
                while let Some(right) = blocks_r.pop() {
                    let mut target = right.value;
                    let mut i = 0;
                    while i < blocks_l.len() {
                        let common = gcd(blocks_l[i].value, target);

                        if common > 1 {
                            let num = blocks_l.remove(i);
                            target /= common;
                            if let Some(result_inner) = result {
                                result = Some(Number::from(result_inner * num));
                            } else {
                                result = Some(num);
                            }

                            if let Some(result_inner) = &result {
                                if result_inner.value.is_multiple_of(right.value) {
                                    result =
                                        Some(Number::from(result_inner.clone() / right.clone()));
                                    break;
                                }
                            }
                        } else {
                            i += 1;
                        }
                    }
                }

                if let Some(result) = result {
                    blocks_l
                        .into_iter()
                        .fold(result, |acc, n| Number::from(acc * n))
                } else {
                    blocks_l
                        .into_iter()
                        .reduce(|n, acc| Number::from(n * acc))
                        .unwrap()
                }
            }
        }
    }

    pub fn as_tree(&self) -> String {
        fn build(num: &Number) -> (Vec<String>, usize) {
            let Some(op) = &num.op else {
                let leaf = format!("{}", num.value);
                let leaflen = leaf.len();
                return (vec![leaf], leaflen / 2);
            };
            let (left, right) = (&op.operands.0, &op.operands.1);
            let op_sym = format!(" {} ", &op.operator);

            let (left_lines, left_mid) = build(left);
            let (right_lines, right_mid) = build(right);

            let left_width = left_lines
                .iter()
                .map(|s| s.chars().count())
                .max()
                .unwrap_or(0);
            let right_width = right_lines
                .iter()
                .map(|s| s.chars().count())
                .max()
                .unwrap_or(0);
            let gap = 4;
            let total_width = left_width + gap + right_width;

            let right_mid = right_mid + gap + left_width;
            let center = (left_mid + right_mid) / 2;

            let line1 = format!(
                "{:total_width$}",
                format!(
                    "{left_spaces}{: ^root_width$}",
                    num.value,
                    left_spaces = " ".repeat(left_mid + 1),
                    root_width = right_mid - left_mid - 1,
                )
            );

            let line2 = format!(
                "{:total_width$}",
                format!(
                    "{}╭{op_sym:─^width$}╮",
                    " ".repeat(left_mid),
                    width = right_mid - left_mid - 1,
                )
            )
            .chars()
            .collect::<Vec<_>>();

            let max_lines = left_lines.len().max(right_lines.len());
            let mut merged: Vec<String> = Vec::new();
            for i in 0..max_lines {
                let left_part = if i < left_lines.len() {
                    &left_lines[i]
                } else {
                    ""
                };
                let right_part = if i < right_lines.len() {
                    &right_lines[i]
                } else {
                    ""
                };
                merged.push(format!(
                    "{left_part:^left_width$}{gaps}{right_part:^right_width$}",
                    gaps = " ".repeat(gap),
                ));
            }

            let mut result = Vec::new();
            result.push(line1);
            result.push(line2.iter().collect());
            result.extend(merged);
            (result, center)
        }
        build(&self).0.join("\n")
    }
    pub fn depth(&self) -> u8 {
        fn helper(num: &Number) -> u8 {
            if let Some(op) = &num.op {
                helper(&op.operands.0).max(helper(&op.operands.1)) + 1
            } else {
                1
            }
        }
        helper(self)
    }
    pub fn as_list(&self) -> String {
        fn build(num: &Number) -> Vec<String> {
            if let Some(op) = &num.op {
                let mut list = vec![];
                if op.operands.0.op.is_some() {
                    let left_list = build(&op.operands.0);
                    list.extend(left_list);
                }
                if op.operands.1.op.is_some() {
                    let right_list = build(&op.operands.1);
                    list.extend(right_list);
                }
                list.push(format!("{} = {}", op, num.value));
                list
            } else {
                vec![format!("{}", num.value)]
            }
        }
        build(&self).join("\n")
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[inline(always)]
fn get_new_numbers(
    i1: usize,
    i2: usize,
    operation: Operation,
    numbers: &[Number],
    target: u32,
    scoreboard: &Scoreboard,
) -> (Vec<Number>, u32) {
    let num = Number::from(operation);
    let value = num.value;
    let score = target.abs_diff(num.value);
    scoreboard.insert_if_better_or_same(score, num.clone());

    let mut new_numbers = Vec::with_capacity(numbers.len() - 1);
    new_numbers.push(num);
    for (i, n) in numbers.iter().enumerate() {
        if i != i1 && i != i2 {
            new_numbers.push(n.clone());
        }
    }
    (new_numbers, value)
}

#[inline(always)]
fn get_hash<T: Hash + ?Sized>(thing: &T) -> u64 {
    let mut s = FxHasher::default();
    thing.hash(&mut s);
    s.finish()
}

pub fn _solve(target: u32, numbers: Vec<Number>, scoreboard: &Scoreboard, visited: &DashSet<u64>) {
    let key = {
        let mut key: u64 = 0;
        for num in &numbers {
            key = key.wrapping_add(get_hash(num));
        }
        key
    };

    if !visited.insert(key) {
        return;
    }

    if numbers.len() >= 4 {
        numbers.par_iter().enumerate().for_each(|(i1, n1)| {
            for (i2, n2) in numbers.iter().enumerate().skip(i1 + 1) {
                run_ops(i1, i2, n1, n2, target, &numbers, scoreboard, visited);
            }
        });
    } else {
        for (i1, n1) in numbers.iter().enumerate().take(numbers.len() - 1) {
            for (i2, n2) in numbers.iter().enumerate().skip(i1 + 1) {
                run_ops(i1, i2, n1, n2, target, &numbers, scoreboard, visited);
            }
        }
    }
}

fn run_ops(
    i1: usize,
    i2: usize,
    n1: &Number,
    n2: &Number,
    target: u32,
    numbers: &[Number],
    scoreboard: &Scoreboard,
    visited: &DashSet<u64>,
) {
    let (n1, n2) = if n1.value < n2.value {
        (n2, n1)
    } else {
        (n1, n2)
    };

    let apply_op = |operation: Operation| {
        let (new_numbers, res) = get_new_numbers(i1, i2, operation, numbers, target, scoreboard);
        if res != target && new_numbers.len() >= 2 {
            _solve(target, new_numbers, scoreboard, visited);
        }
    };

    apply_op(n1.clone() + n2.clone());
    if n1.value != n2.value {
        apply_op(n1.clone() - n2.clone());
    }
    if n2.value != 1 {
        apply_op(n1.clone() * n2.clone());
    }
    if n2.value != 1 && n1.value % n2.value == 0 {
        apply_op(n1.clone() / n2.clone());
    }
}

pub fn solve(target: u32, numbers: Vec<u32>) -> Scoreboard {
    let scoreboard = Scoreboard::new();
    let mut numbers: Vec<Number> = numbers.into_iter().map(Number::new).collect();
    numbers.sort_by_key(|n| std::cmp::Reverse(n.value));

    let visited = DashSet::default();

    for num in &numbers {
        let score = target.abs_diff(num.value);
        scoreboard.insert_if_better_or_same(score, num.clone());
    }

    _solve(target, numbers, &scoreboard, &visited);
    scoreboard
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn commutative_operations_result_the_same_hash_regardless_of_order() {
        let n1 = Number::new(5);
        let n2 = Number::new(10);

        let op_add_1 = Operation::new(Op::Add, n1.clone(), n2.clone());
        let op_add_2 = Operation::new(Op::Add, n2.clone(), n1.clone());

        assert_eq!(
            op_add_1, op_add_2,
            "Addition should be equal regardless of order"
        );
        assert_eq!(
            get_hash(&op_add_1),
            get_hash(&op_add_2),
            "Addition hashes should be identical regardless of order"
        );

        let op_mul_1 = Operation::new(Op::Mul, n1.clone(), n2.clone());
        let op_mul_2 = Operation::new(Op::Mul, n2.clone(), n1.clone());

        assert_eq!(
            op_mul_1, op_mul_2,
            "Multiplication should be equal regardless of order"
        );
        assert_eq!(
            get_hash(&op_mul_1),
            get_hash(&op_mul_2),
            "Multiplication hashes should be identical regardless of order"
        );
    }

    #[test]
    fn similar_operators_result_equal_operations() {
        // (10 - 2) + 3 = 11  ==  (10 + 3) - 2 = 11
        let n1 = Number::new(10);
        let n2 = Number::new(2);
        let n3 = Number::new(3);

        // (10 - 2) + 3 = 11
        let op_sub = Operation::new(Op::Sub, n1.clone(), n2.clone());
        let n = Number::from(op_sub);
        let op_final_1 = Operation::new(Op::Add, n, n3.clone());

        // (10 + 3) - 2 = 11
        let op_add = Operation::new(Op::Add, n1.clone(), n3.clone());
        let n = Number::from(op_add);
        let op_final_2 = Operation::new(Op::Sub, n, n2.clone());

        assert_eq!(
            op_final_1, op_final_2,
            "Mixed add/sub resulting in same blocks should be equal"
        );
    }

    #[test]
    fn two_numbers_are_not_the_same_when_the_way_deriving_them_is_different() {
        // 2 + 2 = 4  !=  2 x 2 = 4
        let n1 = Number::new(2);
        let n2 = Number::new(2);

        let op_add = Operation::new(Op::Add, n1.clone(), n2.clone());
        let res1 = Number::from(op_add);

        let op_mul = Operation::new(Op::Mul, n1.clone(), n1.clone());

        let res2 = Number::from(op_mul);

        assert_ne!(
            res1, res2,
            "Mixed add/sub resulting in same blocks should be equal"
        );
    }
}
