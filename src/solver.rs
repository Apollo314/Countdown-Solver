use std::{
    collections::BTreeMap,
    fmt::{Debug, Display},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, PartialEq)]
pub struct Operation {
    operator: Operator,
    operands: (Number, Number),
}

#[derive(Clone, PartialEq)]
pub struct Number {
    pub value: i32,
    op: Option<Box<Operation>>,
    pub depth: i32,
}

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

        let left_width = left_lines.iter().map(|s| s.len()).max().unwrap_or(0);
        let right_width = right_lines.iter().map(|s| s.len()).max().unwrap_or(0);
        let gap = 6;
        let total_width = left_width + gap + right_width;

        let root = format!("{}", self.value);

        let right_mid = right_mid + gap + left_width;
        let center = (left_mid + right_mid) / 2;

        let l1 = format!(
            "{:total_width$}",
            format!(
                "{left_spaces}{root:_^root_width$}",
                left_spaces = " ".repeat(left_mid + 1),
                root_width = right_mid - left_mid - 1,
            )
        );

        let mut line2: Vec<char> = vec![' '; total_width];
        if left_mid < total_width {
            line2[left_mid] = '/';
        }
        if right_mid < total_width {
            line2[right_mid] = '\\';
        }
        line2[center] = op_sym;

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
                "{:^width_left$}{}{:^width_right$}",
                left_part,
                " ".repeat(gap),
                right_part,
                width_left = left_width,
                width_right = right_width
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

fn distance(target: i32, result: i32) -> i32 {
    (target - result).abs()
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
    numbers: Vec<Number>,
    target: i32,
    scoreboard: &mut BTreeMap<i32, Vec<Number>>,
    depth: i32,
) -> Vec<Number> {
    let res = get_res(&operation);
    let num_depth = operation.operands.0.depth + operation.operands.1.depth + 1;
    let num = Number {
        value: res,
        op: Some(Box::new(operation)),
        depth: num_depth,
    };
    if depth == num_depth {
        let score = distance(target, res);
        let solutions = scoreboard.entry(score).or_default();
        solutions.push(num.clone());
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
    scoreboard: &mut BTreeMap<i32, Vec<Number>>,
    depth: i32,
) {
    // if it just so happens that one of the numbers is the target.
    if depth == 0 {
        for num in &numbers {
            if num.value == target {
                scoreboard.entry(0).or_default().push(num.clone());
            }
        }
    }
    for (i1, n1) in numbers.iter().enumerate() {
        let num1 = n1.value;
        for (i2, n2) in (i1 + 1..).zip(numbers[i1 + 1..].iter()) {
            let num2 = n2.value;
            let new_numbers = get_new_numbers(
                i1,
                i2,
                Operation {
                    operator: Operator::Add,
                    operands: (n1.clone(), n2.clone()),
                },
                numbers.clone(),
                target,
                scoreboard,
                depth + 1,
            );
            _solve(target, new_numbers, scoreboard, depth + 1);
            if num1 != 1 && num2 != 1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Mul,
                        operands: (n1.clone(), n2.clone()),
                    },
                    numbers.clone(),
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1);
            }
            if num1 >= num2 && num1 % num2 == 0 && num2 != 0 && num2 != 1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Div,
                        operands: (n1.clone(), n2.clone()),
                    },
                    numbers.clone(),
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1);
            } else if num2 > num1 && num2 % num1 == 0 && num1 != 0 && num1 != 1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Div,
                        operands: (n2.clone(), n1.clone()),
                    },
                    numbers.clone(),
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1);
            }
            if num1 > num2 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Sub,
                        operands: (n1.clone(), n2.clone()),
                    },
                    numbers.clone(),
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1);
            } else if num2 > num1 {
                let new_numbers = get_new_numbers(
                    i1,
                    i2,
                    Operation {
                        operator: Operator::Sub,
                        operands: (n2.clone(), n1.clone()),
                    },
                    numbers.clone(),
                    target,
                    scoreboard,
                    depth + 1,
                );
                _solve(target, new_numbers, scoreboard, depth + 1);
            }
        }
    }
}

pub fn solve(target: i32, numbers: Vec<i32>) -> BTreeMap<i32, Vec<Number>> {
    let mut scoreboard = BTreeMap::new();
    let numbers = numbers
        .iter()
        .map(|num| Number {
            value: *num,
            op: None,
            depth: 0,
        })
        .collect();
    _solve(target, numbers, &mut scoreboard, 0);
    scoreboard
}
