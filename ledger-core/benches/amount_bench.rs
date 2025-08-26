use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn amount_addition_benchmark(c: &mut Criterion) {
    c.bench_function("amount addition", |b| {
        b.iter(|| {
            // TODO: Implement actual amount addition benchmark
            black_box(1 + 1)
        })
    });
}

criterion_group!(benches, amount_addition_benchmark);
criterion_main!(benches);
