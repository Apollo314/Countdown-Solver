use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::{
    collections::{BTreeMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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
        match &self.operator {
            Operator::Add => {
                let (op11, op12) = &self.operands;
                let (op21, op22) = &other.operands;
                (op11 == op21 || op11 == op22)
                    && (op11.value + op12.value == op21.value + op22.value)
            }
            Operator::Mul => {
                let (op11, op12) = &self.operands;
                let (op21, op22) = &other.operands;
                (op11 == op21 || op11 == op22)
                    && (op11.value * op12.value == op21.value * op22.value)
            }
            Operator::Sub => {
                let (op11, op12) = &self.operands;
                let (op21, op22) = &other.operands;
                (op11 == op21) && (op12 == op22)
            }
            Operator::Div => {
                let (op11, op12) = &self.operands;
                let (op21, op22) = &other.operands;
                (op11 == op21) && (op12 == op22)
            }
        }
    }
}

impl Hash for Operation {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.operator.hash(state);

        let (left, right) = &self.operands;

        match self.operator {
            Operator::Add | Operator::Mul => {
                let mut h1 = std::collections::hash_map::DefaultHasher::new();
                let mut h2 = std::collections::hash_map::DefaultHasher::new();

                left.hash(&mut h1);
                right.hash(&mut h2);

                let (a, b) = (h1.finish(), h2.finish());

                if a < b {
                    a.hash(state);
                    b.hash(state);
                } else {
                    b.hash(state);
                    a.hash(state);
                }
            }
            Operator::Sub | Operator::Div => {
                left.hash(state);
                right.hash(state);
            }
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
    fn _as_tree(&self) -> (Vec<String>, usize) {
        if self.op.is_none() {
            return (vec![format!("{}", self.value)], 0);
        }

        let op = self.op.as_ref().unwrap();
        let (left, right) = (&op.operands.0, &op.operands.1);
        let op_sym = format!("{}", &op.operator).chars().nth(0).unwrap();

        let (left_lines, left_mid) = left._as_tree();
        let (right_lines, right_mid) = right._as_tree();

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

        let root = format!("{}", self.value);

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
                "{}╭{:─^width$}╮",
                " ".repeat(left_mid),
                format!(" {op_sym} "),
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
    pub fn as_tree(&self) -> String {
        format!("{}\n", self._as_tree().0.join("\n"))
    }
    fn _as_list(&self) -> Vec<String> {
        if let Some(op) = &self.op {
            let mut list = vec![];
            if op.operands.0.op.is_some() {
                let left_list = op.operands.0._as_list();
                list.extend(left_list);
            }
            if op.operands.1.op.is_some() {
                let right_list = op.operands.1._as_list();
                list.extend(right_list);
            }
            list.push(format!("{} = {}", op, self.value));
            list
        } else {
            vec![format!("{}", self.value)]
        }
    }
    pub fn as_list(&self) -> String {
        format!("{}\n", self._as_list().join("\n"))
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in self._as_tree().0 {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

fn get_res(operation: &Operation) -> i32 {
    match operation.operator {
        Operator::Add => operation.operands.0.value + operation.operands.1.value,
        Operator::Sub => operation.operands.0.value - operation.operands.1.value,
        Operator::Div => operation.operands.0.value / operation.operands.1.value,
        Operator::Mul => operation.operands.0.value * operation.operands.1.value,
    }
}

fn get_new_numbers(
    i1: usize,
    i2: usize,
    operation: Operation,
    numbers: &[Number],
    target: i32,
    scoreboard: &mut Scoreboard,
    depth: i32,
) -> Vec<Number> {
    let res = get_res(&operation);
    let num_depth = operation.operands.0.depth + operation.operands.1.depth + 1;
    let num = Number {
        value: res,
        op: Some(Rc::new(operation)),
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
    numbers: Vec<Number>,
    scoreboard: &mut Scoreboard,
    depth: i32,
    visited: &mut HashSet<Vec<i32>>,
) {
    let mut key = numbers.iter().map(|n| n.value).collect::<Vec<_>>();
    key.sort_unstable();

    if !visited.insert(key) {
        return;
    }
    // if it just so happens that one of the numbers is the target.
    if depth == 0 {
        for num in &numbers {
            if num.value == target {
                scoreboard.entry(0).or_default().insert(num.clone());
            }
        }
    }
    for (i1, n1) in numbers.iter().enumerate() {
        let num1 = n1.value;
        for (i2, n2) in numbers.iter().enumerate().skip(i1 + 1) {
            let num2 = n2.value;
            let new_numbers = get_new_numbers(
                i1,
                i2,
                Operation {
                    operator: Operator::Add,
                    operands: (n1.clone(), n2.clone()),
                },
                &numbers,
                target,
                scoreboard,
                depth + 1,
            );
            _solve(target, new_numbers, scoreboard, depth + 1, visited);
            if num1 != 1 && num2 != 1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Mul,
                        operands: (n1.clone(), n2.clone()),
                    },
                    &numbers,
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1, visited);
            }
            if num1 >= num2 && num1 % num2 == 0 && num2 != 0 && num2 != 1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Div,
                        operands: (n1.clone(), n2.clone()),
                    },
                    &numbers,
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1, visited);
            } else if num2 > num1 && num2 % num1 == 0 && num1 != 0 && num1 != 1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Div,
                        operands: (n2.clone(), n1.clone()),
                    },
                    &numbers,
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1, visited);
            }
            if num1 > num2 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Sub,
                        operands: (n1.clone(), n2.clone()),
                    },
                    &numbers,
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1, visited);
            } else if num2 > num1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Sub,
                        operands: (n2.clone(), n1.clone()),
                    },
                    &numbers,
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1, visited);
            }
        }
    }
}

pub fn solve(target: i32, numbers: Vec<i32>) -> Scoreboard {
    let mut scoreboard = Scoreboard::new();
    let numbers = numbers
        .iter()
        .map(|num| Number {
            value: *num,
            op: None,
            depth: 0,
        })
        .collect();
    let mut visited = HashSet::new();
    _solve(target, numbers, &mut scoreboard, 0, &mut visited);
    scoreboard
}
