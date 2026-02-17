use countdown::solver::solve;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("solve 1 2 3 4 5 6 -> 151", |b| {
        b.iter(|| solve(black_box(151), black_box(vec![1, 2, 3, 4, 5, 6])))
    });

    c.bench_function("solve and print 25 50 75 100 3 6 -> 952", |b| {
        b.iter(|| {
            let solution = solve(black_box(952), black_box(vec![25, 50, 75, 100, 3, 6]));
            for sol in solution.best_solutions.lock().unwrap().iter() {
                _ = black_box(sol.as_list());
            }
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
