use countdown::solver::solve;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("solve 1 2 3 4 5 6 -> 151", |b| {
        b.iter(|| solve(black_box(151), black_box(vec![1, 2, 3, 4, 5, 6])))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
