use clap::{Parser, ValueEnum};
use std::fmt::Display;
mod solver;

#[derive(Parser, Debug)]
pub struct Args {
    #[arg(short, long, required = true, num_args(1..))]
    numbers: Vec<i32>,

    #[arg(short, long, required = true)]
    target: i32,

    #[arg(short, long, required = false, default_value_t = DisplayFormat::Tree, ignore_case = true)]
    display_format: DisplayFormat,

    #[arg(short, long, required = false, default_value_t = 5)]
    max_displayed_solution: usize,
}

#[derive(Copy, Clone, ValueEnum, Debug)]
enum DisplayFormat {
    Tree,
    List,
}

impl Display for DisplayFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DisplayFormat::Tree => "tree",
                DisplayFormat::List => "list",
            }
        )
    }
}

fn main() {
    let args = Args::parse();
    let mut scoreboard = solver::solve(args.target, args.numbers);
    let solutions = scoreboard
        .iter_mut()
        .next()
        .expect("there seems to be no solution");
    solutions.1.sort_by_key(|num| num.depth);
    let show_max = args.max_displayed_solution.min(solutions.1.len());
    println!(
        "for distance of {}, there are {} solutions, here are {}:",
        solutions.0,
        solutions.1.len(),
        show_max
    );
    for j in (0..show_max).rev() {
        match args.display_format {
            DisplayFormat::Tree => {
                println!("{}", solutions.1[j].as_tree());
            }
            DisplayFormat::List => {
                println!("{}", solutions.1[j].as_list());
            }
        }
    }
}
