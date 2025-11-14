use clap::{Parser, ValueEnum};
use std::fmt::Display;
mod solver;

#[derive(Parser, Debug)]
pub struct Args {
    #[arg(short, long, required = true, num_args(1..))]
    numbers: Vec<i32>,

    #[arg(short, long, required = true)]
    target: i32,

    #[arg(
        short = 'l',
        long,
        required = false,
        default_value_t = false,
        help = "display results as list of operations instead of their tree structure"
    )]
    list: bool,

    #[arg(short, long, required = false, default_value_t = 5)]
    max_displayed_solution: usize,

    #[arg(
        short = 's',
        required = false,
        default_value_t = false,
        help = "for printing spaces with same width as the other characters for non-monospace fonts"
    )]
    use_blank_braille_for_space: bool,
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
    let (distance, solutions) = scoreboard
        .iter_mut()
        .next()
        .expect("there seems to be no solution");
    let mut solutions = solutions.iter().collect::<Vec<_>>();
    solutions.sort_by_key(|num| num.depth);
    let show_max = args.max_displayed_solution.min(solutions.len());
    println!(
        "for distance of {distance}, there are {} solutions, here are {}:",
        solutions.len(),
        show_max
    );
    for j in (0..show_max).rev() {
        if args.list {
            let mut list_string = solutions[j].as_list();
            if args.use_blank_braille_for_space {
                list_string = list_string.replace(" ", "⠀");
            }
            println!("{list_string}\n");
        } else {
            let mut tree_string = solutions[j].as_tree();
            if args.use_blank_braille_for_space {
                tree_string = tree_string.replace(" ", "⠀");
            }
            println!("{tree_string}\n");
        }
    }
}
