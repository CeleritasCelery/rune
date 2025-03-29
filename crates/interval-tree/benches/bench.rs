use std::ops::Range;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use interval_tree::{IntervalTree, StackIterator, TextRange};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

fn generate_random_intervals(
    n: usize,
    rng: &mut StdRng,
    start_bound: Range<usize>,
    len_bound: Range<usize>,
) -> Vec<(TextRange, i64)> {
    let mut intervals = Vec::with_capacity(n);
    for _ in 0..n {
        let start = rng.random_range(start_bound.clone());
        let end = start + rng.random_range(len_bound.clone());
        let value = rng.random::<i64>();
        intervals.push((TextRange::new(start, end), value));
    }
    intervals
}

fn generate_random_intervals_default(n: usize, rng: &mut StdRng) -> Vec<(TextRange, i64)> {
    generate_random_intervals(n, rng, 0..5000, 1..100)
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
    let intervals = generate_random_intervals_default(n, &mut rng);

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
    let intervals = generate_random_intervals_default(n, &mut rng);

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
    let intervals = generate_random_intervals(n, &mut rng, 0..10000, 1..100);
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
    let size = tree.size();

    c.bench_function(&format!("find_intersects of {search_n} times in {size} intervals"), |b| {
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
    let intervals = generate_random_intervals(n, &mut rng, 0..10000, 1..100);

    let tree = build_tree(&intervals);

    let min_node = tree.min();
    let min_key = min_node.map(|n| n.key);
    let size = tree.size();
    println!("{:?}", size);

    c.bench_function(&format!("iterating with stack iterator over {size} intervals"), |b| {
        b.iter(|| {
            let mut stack_iter = StackIterator::new(&tree, min_key);
            let mut count = 0;
            while let Some(_n) = stack_iter.next() {
                count += 1;
            }
            assert_eq!(count, size);
            black_box(stack_iter);
        });
    });
}

fn apply_with_split_benchmark(c: &mut Criterion) {
    let mut rng = StdRng::seed_from_u64(42);
    let n = 10000;

    // Benchmark applying to small ranges (affecting few intervals)
    c.bench_function(&format!("apply_with_split small ranges on {n} intervals"), |b| {
        b.iter(|| {
            let mut tree = IntervalTree::new();
            for i in 0..n {
                let start = i * 10;
                let end = start + 10;
                tree.insert(TextRange::new(start, end), i as i64, |a, _b| a);
            }
            // Apply to 100 small random ranges
            for _ in 0..100 {
                let pos = rng.random_range(0..n * 10);
                tree.apply_with_split(|val| Some(val + 1), TextRange::new(pos, pos + 5));
            }
            black_box(tree);
        });
    });

    // Benchmark applying to large ranges (affecting many intervals)
    c.bench_function(&format!("apply_with_split large ranges on {n} intervals"), |b| {
        b.iter(|| {
            let mut tree = IntervalTree::new();
            for i in 0..n {
                let start = i * 10;
                let end = start + 10;
                tree.insert(TextRange::new(start, end), i as i64, |a, _b| a);
            }
            // Apply to 5 large ranges
            for i in 0..5 {
                let start = i * 2000;
                tree.apply_with_split(|val| Some(val * 2), TextRange::new(start, start + 1000));
            }
            black_box(tree);
        });
    });

    // Benchmark with deletions
    c.bench_function(&format!("apply_with_split deletions on {n} intervals"), |b| {
        b.iter(|| {
            let mut tree = IntervalTree::new();
            for i in 0..n {
                let start = i * 10;
                let end = start + 10;
                tree.insert(TextRange::new(start, end), i as i64, |a, _b| a);
            }
            // Delete 100 small random ranges
            for _ in 0..100 {
                let pos = rng.random_range(0..n * 10);
                tree.apply_with_split(|_| None, TextRange::new(pos, pos + 5));
            }
            black_box(tree);
        });
    });
}

fn clean_benchmark(c: &mut Criterion) {
    let n = 10000;

    // Build a tree with many adjacent intervals that can be merged
    let mut tree = IntervalTree::new();
    for i in 0..n {
        let start = i * 10;
        let end = start + 10;
        // Make every other interval have the same value to test merging
        let value = if i % 5 == 0 { 1 } else { 2 };
        tree.insert(TextRange::new(start, end), value, |a, _b| a);
    }

    c.bench_function(&format!("clean {n} intervals with mergeable values"), |b| {
        b.iter(|| {
            let mut tree = tree.clone();
            tree.clean(|a, b| a == b, |_| false);
            black_box(tree);
        });
    });

    // Also benchmark with some empty intervals
    let mut tree_with_empty = IntervalTree::new();
    for i in 0..n {
        let start = i * 10;
        let end = start + 10;
        // Make some intervals empty
        let value = if i % 5 == 0 { 0 } else { 1 };
        tree_with_empty.insert(TextRange::new(start, end), value, |a, _b| a);
    }

    c.bench_function(&format!("clean {n} intervals with empty values"), |b| {
        b.iter(|| {
            let mut tree = tree_with_empty.clone();
            tree.clean(|a, b| a == b, |v| *v == 0);
            black_box(tree);
        });
    });
}

criterion_group!(
    benches,
    insertion_benchmark,
    deletion_benchmark,
    find_intersects_benchmark,
    iterator_benchmark,
    clean_benchmark,
    apply_with_split_benchmark
);
criterion_main!(benches);
