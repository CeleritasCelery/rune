use criterion::{Criterion, black_box, criterion_group, criterion_main};
use interval_tree::{IntervalTree, RawPointerIterator, StackIterator, TextRange};
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

fn build_tree(intervals: &[(TextRange, i64)]) -> IntervalTree<i64> {
    let mut tree = IntervalTree::new();
    for (range, value) in intervals {
        tree.insert(*range, *value, |a, _b| a);
    }
    tree
}

fn insertion_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 50000;
    let intervals = generate_random_intervals(10000, &mut rng);

    c.bench_function(&format!("insert {n} random intervals"), |b| {
        b.iter(|| {
            let tree = build_tree(&intervals);
            black_box(tree);
        });
    });
}

fn deletion_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 10000;
    let intervals = generate_random_intervals(n, &mut rng);

    let tree = build_tree(&intervals);

    c.bench_function(&format!("delete {n} random intervals"), |b| {
        b.iter(|| {
            let mut tree = tree.clone();
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

    let tree = build_tree(&intervals);

    c.bench_function(&format!("find_intersects of {search_n} times in {n} intervals"), |b| {
        b.iter(|| {
            // Search with all large ranges
            for search_range in &search_ranges {
                let results = tree.find_intersects(*search_range);
                black_box(results);
            }
        });
    });
}

fn iterator_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 100000;
    let intervals = generate_random_intervals(n, &mut rng);

    let tree = build_tree(&intervals);

    let min_node = tree.min();
    let min_key = min_node.map(|n| n.key);

    c.bench_function(&format!("iterating with stack iterator over {n} intervals"), |b| {
        b.iter(|| {
            let mut stack_iter = StackIterator::new(&tree, min_key);
            while let Some(_n) = stack_iter.next() {}
            black_box(stack_iter);
        });
    });

    c.bench_function(&format!("iterating with raw pointer iterator over {n} intervals"), |b| {
        b.iter(|| {
            let mut raw_iter = RawPointerIterator::new(&tree, min_key);
            while let Some(_n) = raw_iter.next() {}
            black_box(raw_iter);
        });
    });
}

criterion_group!(
    benches,
    insertion_benchmark,
    deletion_benchmark,
    find_intersects_benchmark,
    iterator_benchmark
);
criterion_main!(benches);
