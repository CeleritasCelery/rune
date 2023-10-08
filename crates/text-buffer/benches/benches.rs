use crdt_testdata::{TestData, TestPatch};
use criterion::BenchmarkId as id;
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use text_buffer::Buffer;

fn get_test_data(name: &str) -> TestData {
    let path = format!(
        "{}/reference-tests/crdt-testdata/data/{}.json.gz",
        env!("CARGO_MANIFEST_DIR"),
        name
    );
    crdt_testdata::load_testing_data(&path)
}

fn realworld(c: &mut Criterion) {
    const DATASETS: &[&str] = &[
        "automerge-paper",
        "rustcode",
        "sveltecomponent",
        "seph-blog1",
        "friendsforever_flat",
    ];
    for name in DATASETS {
        let mut group = c.benchmark_group("realworld");
        let test_data = get_test_data(name);

        let len = test_data
            .txns
            .iter()
            .flat_map(|txn| txn.patches.iter())
            .map(|patch| patch.1 + patch.2.len())
            .sum::<usize>();

        group.throughput(Throughput::Elements(len as u64));

        group.bench_function(id::new("direct", name), |b| {
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
        // because we create the buffer from a String, the gap size will be 0,
        // meaning any insert will resize it

        let string = "a".repeat(*size);
        group.bench_function(id::new("create", size), |b| {
            b.iter(|| black_box(Buffer::from(string.clone())));
        });
        // subtract create from resize to get total cost
        group.bench_function(id::new("resize", size), |b| {
            b.iter(|| {
                let buffer = &mut Buffer::from(string.clone());
                buffer.insert_char('a');
            });
        });
    }
    group.finish();
}

fn move_gap(c: &mut Criterion) {
    let mut group = c.benchmark_group("move_gap");

    for (size, sample) in &[(10, 100), (15, 100), (20, 50), (25, 10), (30, 10)] {
        group.sample_size(*sample);
        let size = &usize::pow(2, *size);
        let id = id::from_parameter(size);
        let string = "a".repeat(*size);
        group.bench_function(id, |b| {
            let buffer = &mut Buffer::from(&*string);
            b.iter(|| buffer.benchmark_move_gap());
        });
    }
    group.finish();
}

fn build_metrics(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_metrics");

    for (size, sample) in &[(10, 100), (15, 100), (20, 50), (25, 10), (30, 10)] {
        group.sample_size(*sample);
        let size = &usize::pow(2, *size);
        let id = id::from_parameter(size);
        group.bench_function(id, |b| {
            let string = "a".repeat(*size);
            b.iter(|| Buffer::benchmark_build_metrics(&string));
        });
    }
    group.finish();
}

criterion_group!(benches, realworld, resize, move_gap, build_metrics);
criterion_main!(benches);
