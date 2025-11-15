use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::{
    collections::{BTreeMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone)]
pub struct Operation {
    operator: Operator,
    operands: (Number, Number),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Number {
    pub value: i32,
    op: Option<Rc<Operation>>,
    pub depth: i32,
}

impl Eq for Operation {}

impl PartialEq for Operation {
    fn eq(&self, other: &Self) -> bool {
        let (mut left_blocks, mut right_blocks) = get_building_blocks(self);
        left_blocks.sort_by_key(|n| n.value);
        right_blocks.sort_by_key(|n| n.value);

        let (mut left_blocks_other, mut right_blocks_other) = get_building_blocks(other);
        left_blocks_other.sort_by_key(|n| n.value);
        right_blocks_other.sort_by_key(|n| n.value);

        left_blocks == left_blocks_other && right_blocks == right_blocks_other
    }
}

pub fn get_building_blocks(op: &Operation) -> (Vec<&Number>, Vec<&Number>) {
    fn is_similar(op: Operator, op2: Operator) -> bool {
        matches!(
            (op, op2),
            (Operator::Add | Operator::Sub, Operator::Add | Operator::Sub)
                | (Operator::Mul | Operator::Div, Operator::Mul | Operator::Div)
        )
    }
    let (left, right) = &op.operands;

    // for mul and div left blocks are numerator, for add and sub they are positives
    let mut left_blocks = vec![];
    // for mul and div right blocks are denominator, for add and sub they are negatives
    let mut right_blocks = vec![];

    if let Some(left_op) = &left.op
        && is_similar(left_op.operator, op.operator)
    {
        let (left_left_blocks, left_right_blocks) = get_building_blocks(left_op);
        left_blocks.extend(left_left_blocks);
        right_blocks.extend(left_right_blocks);
    } else {
        left_blocks.push(left);
    }
    if let Some(right_op) = &right.op
        && is_similar(right_op.operator, op.operator)
    {
        let (right_left_blocks, right_right_blocks) = get_building_blocks(right_op);
        match &op.operator {
            Operator::Mul | Operator::Add => {
                left_blocks.extend(right_left_blocks);
                right_blocks.extend(right_right_blocks);
            }
            Operator::Sub | Operator::Div => {
                right_blocks.extend(right_left_blocks);
                left_blocks.extend(right_right_blocks);
            }
        }
    } else {
        match &op.operator {
            Operator::Mul | Operator::Add => {
                left_blocks.push(right);
            }
            Operator::Sub | Operator::Div => {
                right_blocks.push(right);
            }
        }
    }

    (left_blocks, right_blocks)
}

impl Hash for Operation {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.operator {
            Operator::Add | Operator::Sub => Operator::Add.hash(state),
            Operator::Mul | Operator::Div => Operator::Mul.hash(state),
        }

        let (mut left_blocks, mut right_blocks) = get_building_blocks(self);
        left_blocks.sort_by_key(|n| n.value);
        right_blocks.sort_by_key(|n| n.value);

        for num in left_blocks {
            num.hash(state);
        }

        for num in right_blocks {
            num.hash(state);
        }
    }
}

pub type Scoreboard = BTreeMap<i32, HashSet<Number>>;

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operator_string = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "x",
            Operator::Div => "÷",
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

use std::fmt;

impl Number {
    pub fn as_tree(&self) -> String {
        fn build(num: &Number) -> (Vec<String>, usize) {
            let Some(op) = &num.op else {
                return (vec![format!("{}", num.value)], 0);
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
            let gap = 6;
            let total_width = left_width + gap + right_width;

            let root = format!("{}", num.value);

            let right_mid = right_mid + gap + left_width;
            let center = (left_mid + right_mid) / 2;

            let l1 = format!(
                "{:total_width$}",
                format!(
                    "{left_spaces}{root: ^root_width$}",
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
            result.push(l1);
            result.push(line2.iter().collect());
            result.extend(merged);
            (result, center)
        }
        build(self).0.join("\n")
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
        build(self).join("\n")
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

fn get_res(
    Operation {
        operator,
        operands: (left, right),
    }: &Operation,
) -> i32 {
    match operator {
        Operator::Add => left.value + right.value,
        Operator::Sub => left.value - right.value,
        Operator::Div => left.value / right.value,
        Operator::Mul => left.value * right.value,
    }
}

fn get_new_numbers(
    i1: usize,
    i2: usize,
    operation: Rc<Operation>,
    numbers: &[Number],
    target: i32,
    scoreboard: &mut Scoreboard,
    depth: i32,
) -> Vec<Number> {
    let res = get_res(&operation);
    let num_depth = operation.operands.0.depth + operation.operands.1.depth + 1;
    let num = Number {
        value: res,
        op: Some(operation),
        depth: num_depth,
    };
    if depth == num_depth {
        let score = (target - res).abs();
        let solutions = scoreboard.entry(score).or_default();
        solutions.insert(num.clone());
    }
    let mut new_numbers = numbers
        .iter()
        .enumerate()
        .filter_map(|(i, num)| {
            if i != i1 && i != i2 {
                Some(num.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    new_numbers.push(num);
    new_numbers
}

pub fn _solve(
    target: i32,
    numbers: Rc<Vec<Number>>,
    scoreboard: &mut Scoreboard,
    depth: i32,
    visited: &mut HashSet<u64>,
) {
    let key = numbers
        .iter()
        .map(|num| {
            let mut s = FxHasher::default();
            num.hash(&mut s);
            s.finish()
        })
        .reduce(|acc, hash| acc.wrapping_mul(hash))
        .unwrap_or(0);

    if !visited.insert(key) {
        return;
    }

    // if it just so happens that one of the numbers is the target.
    if depth == 0 {
        for num in numbers.iter() {
            if num.value == target {
                scoreboard.entry(0).or_default().insert(num.clone());
                return;
            }
        }
    }
    let operators = [Operator::Add, Operator::Mul, Operator::Sub, Operator::Div];
    for (i1, n1) in numbers.iter().enumerate() {
        for (i2, n2) in numbers.iter().enumerate().skip(i1 + 1) {
            let (mut n1, mut n2) = (n1, n2);
            if n1.value < n2.value {
                std::mem::swap(&mut n1, &mut n2);
            }
            for &operator in &operators {
                if (operator == Operator::Mul && (n1.value == 1 || n2.value == 1))
                    || (operator == Operator::Div
                        && (n1.value == 1 || n2.value == 1 || n1.value % n2.value != 0))
                    || (operator == Operator::Sub && n1.value == n2.value)
                {
                    continue;
                }
                let operation = Rc::new(Operation {
                    operator,
                    operands: (n1.clone(), n2.clone()),
                });
                let new_numbers = Rc::new(get_new_numbers(
                    i1,
                    i2,
                    operation.clone(),
                    &numbers,
                    target,
                    scoreboard,
                    depth + 1,
                ));
                _solve(target, new_numbers, scoreboard, depth + 1, visited);
            }
        }
    }
}

pub fn solve(target: i32, numbers: Vec<i32>) -> Scoreboard {
    let mut scoreboard = Scoreboard::new();
    let numbers = Rc::new(
        numbers
            .iter()
            .map(|num| Number {
                value: *num,
                op: None,
                depth: 0,
            })
            .collect(),
    );
    let mut visited = HashSet::new();
    _solve(target, numbers, &mut scoreboard, 0, &mut visited);
    scoreboard
}
