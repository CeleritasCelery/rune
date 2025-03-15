use criterion::{Criterion, black_box, criterion_group, criterion_main};
use interval_tree::{IntervalTree, TextRange};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

fn generate_random_intervals(n: usize, rng: &mut StdRng) -> Vec<(TextRange, i64)> {
    let mut intervals = Vec::with_capacity(n);
    for _ in 0..n {
        let start = rng.random_range(0..1000);
        let end = start + rng.random_range(1..100);
        let value = rng.random::<i64>();
        intervals.push((TextRange::new(start, end), value));
    }
    intervals
}

fn insertion_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 50000;
    let intervals = generate_random_intervals(10000, &mut rng);

    c.bench_function(&format!("insert {n} random intervals"), |b| {
        b.iter(|| {
            let mut tree = IntervalTree::new();
            for (range, value) in &intervals {
                tree.insert(*range, *value, |a, _b| a);
            }
            black_box(tree);
        });
    });
}

fn deletion_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 10000;
    let intervals = generate_random_intervals(n, &mut rng);
    env_logger::init();

    c.bench_function(&format!("delete {n} random intervals"), |b| {
        b.iter(|| {
            let mut tree = IntervalTree::new();
            // Insert all intervals
            for (range, value) in &intervals {
                tree.insert(*range, *value, |a, _b| a);
            }
            // Delete all intervals
            for (range, _) in &intervals {
                tree.delete(*range, false);
            }
            black_box(tree);
        });
    });
}

fn find_intersects_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 50000;
    let intervals = generate_random_intervals(n, &mut rng);
    // Generate 100 large search ranges
    let search_n = 1000;
    let search_ranges: Vec<TextRange> = (0..search_n)
        .map(|_| {
            let start = rng.random_range(0..200);
            let end = start + rng.random_range(500..700);
            TextRange::new(start, end)
        })
        .collect();

    c.bench_function(&format!("find_intersects of {search_n} times in {n} intervals"), |b| {
        b.iter(|| {
            let mut tree = IntervalTree::new();
            // Insert all intervals
            for (range, value) in &intervals {
                tree.insert(*range, *value, |a, _b| a);
            }
            // Search with all large ranges
            for search_range in &search_ranges {
                let results = tree.find_intersects(*search_range);
                black_box(results);
            }
        });
    });
}

criterion_group!(benches, insertion_benchmark, deletion_benchmark, find_intersects_benchmark);
criterion_main!(benches);
