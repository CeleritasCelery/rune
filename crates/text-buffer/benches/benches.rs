use crdt_testdata::{TestData, TestPatch};
use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput,
};
use text_buffer::Buffer;

fn get_test_data(name: &str) -> TestData {
    let path = format!(
        "{}/reference-tests/crdt-testdata/data/{}.json.gz",
        env!("CARGO_MANIFEST_DIR"),
        name
    );
    println!("loading {}", path);
    crdt_testdata::load_testing_data(&path)
}

fn real_world(c: &mut Criterion) {
    const DATASETS: &[&str] = &["automerge-paper", "rustcode", "sveltecomponent", "seph-blog1"];
    for name in DATASETS {
        let mut group = c.benchmark_group("testdata");
        let test_data = get_test_data(name);

        let len = test_data
            .txns
            .iter()
            .flat_map(|txn| txn.patches.iter())
            .map(|patch| patch.1 + patch.2.len())
            .sum::<usize>();

        group.throughput(Throughput::Elements(len as u64));

        group.bench_function(BenchmarkId::new("direct", name), |b| {
            b.iter(|| {
                let mut buffer = Buffer::from(&*test_data.start_content);
                for txn in test_data.txns.iter() {
                    for TestPatch(pos, del, ins) in &txn.patches {
                        buffer.set_cursor(*pos);
                        buffer.delete_forwards(*del);
                        buffer.insert(ins);
                    }
                }

                assert_eq!(buffer.len(), test_data.end_content.len());
                black_box(buffer.len_chars());
            })
        });
    }
}

fn resize(c: &mut Criterion) {
    let mut group = c.benchmark_group("resize");

    for (size, sample) in &[(10, 100), (15, 100), (20, 50), (25, 10), (30, 10)] {
        group.sample_size(*sample);
        let size = &usize::pow(2, *size);
        let id = BenchmarkId::from_parameter(size);
        // because we create the buffer from a String, the gap size will be 0,
        // meaning any insert will resize it
        group.bench_function(id, |b| {
            b.iter_batched(
                || {
                    let string: String = std::iter::repeat('a').take(*size).collect();
                    Buffer::from(string)
                },
                |mut buffer| {
                    buffer.insert_char('a');
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn move_gap(c: &mut Criterion) {
    let mut group = c.benchmark_group("move_gap");

    for size in &[8, 9, 10, 11, 12, 13, 14, 15, 16, 17] {
        let size = &usize::pow(2, *size);
        let id = BenchmarkId::from_parameter(size);
        group.bench_function(id, |b| {
            let string: String = std::iter::repeat('a').take(*size).collect();
            b.iter_batched(
                || {
                    // create from a reference
                    let mut buffer = Buffer::with_gap(*size);
                    buffer.insert(&*string);
                    buffer
                },
                |mut buffer| {
                    let len = buffer.len_chars();
                    buffer.set_cursor(len);
                    buffer.insert_char('a');
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn move_cursor(c: &mut Criterion) {
    let mut group = c.benchmark_group("move_cursor");

    for (size, sample) in &[(10, 100), (15, 100), (20, 50), (25, 10), (30, 10)] {
        group.sample_size(*sample);
        let size = &usize::pow(2, *size);
        let id = BenchmarkId::from_parameter(size);
        group.bench_function(id, |b| {
            let string: String = std::iter::repeat('a').take(*size).collect();
            let buffer = &mut Buffer::from(&*string);
            b.iter(|| buffer.benchmark_move_gap());
        });
    }
    group.finish();
}

criterion_group!(benches, real_world, resize, move_cursor, move_gap);
criterion_main!(benches);
