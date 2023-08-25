use crdt_testdata::{TestData, TestPatch};
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
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
                let mut buffer = Buffer::from(test_data.start_content.as_str());
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

criterion_group!(benches, real_world);
criterion_main!(benches);
