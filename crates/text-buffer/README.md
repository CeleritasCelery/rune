# Text Buffer
An implementation of a gap buffer for use in rune.

## benchmarks
Basic benchmarks located under `/benches` directory. Run with `cargo bench`

## fuzzing
Fuzzer located at `fuzz/fuzz_targets/fuzz_buffers.rs`. After installing [cargo fuzz](https://crates.io/crates/cargo-fuzz), run with `cargo +nightly fuzz run fuzz_buffers`. Note that the same file has a function `create_repo` to automatically create a reproduction test of the fuzzer output. Add these to the unit tests.

## reference tests
Reference tests are located at `reference-tests/src/main.rs`. Change to that directory and run with `cargo test`. These are a set of [editing traces](https://github.com/josephg/editing-traces) developed by the author of [jumprope-rs](https://github.com/josephg/jumprope-rs).

If failures are encountered, you can run against the reference javascript implementation to get the exact failing change with `reference-tests/run.py`.
