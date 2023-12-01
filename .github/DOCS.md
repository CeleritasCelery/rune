# Github config and workflows

In this folder there is configuration for codecoverage, dependabot, and ci
workflows that check the library more deeply than the default configurations. This folder was merged <https://github.com/jonhoo/rust-ci-conf/> which provides a reasonably sensible base for writing your own ci on. 

An overview of the files in this project is available at:
<https://www.youtube.com/watch?v=xUH-4y92jPg&t=491s>

## Potential additions

Taken from the template above, there are different workflows that wouldn't make sense for the `rune` project. I'll document them here in case we want to change our minds in the future and end up using them.

### cargo-hack
> `cargo-hack` checks combinations of feature flags to ensure that features are all additive which is required for feature unification.

[source](https://github.com/jonhoo/rust-ci-conf/blob/main/.github/workflows/check.yml#L77)

As we don't currently have any meaningful feature gates.

### scheduled
> Run scheduled (rolling) jobs on a nightly basis, as your crate may break independently of any given PR. E.g., updates to rust nightly and updates to this crates dependencies. See check.yml for information about how the concurrency cancelation and workflow triggering works

[source](https://github.com/jonhoo/rust-ci-conf/blob/main/.github/workflows/scheduled.yml#L1)

As we don't run on nightly, we don't really need to check whether nightly breaks our builds. A potential argument for adding this could be that we are running nightly for the CI, and we could break the CI from nightly builds. I propose revisiting adding the `scheduled.yml` linked above if that's the case.

### loom
> Loom is a testing tool for concurrent Rust code. It runs a test many times, permuting the possible concurrent executions of that test under the C11 memory model. It uses state reduction techniques to avoid combinatorial explosion.

[source](https://crates.io/crates/loom)

Loom is great and it's backed by Tokio, but it would mean a bigger investment in making the threads we use be loom specific. Definitely something to iterate on, as we get onto Async Emacs.

### codecov.io
> Enhance Your Testing the Codecov Way: Codecov is the all-in-one code coverage reporting solution for any test suite — giving developers actionable insights to deploy reliable code with confidence.

[source](https://about.codecov.io), [repo of Github Action](https://github.com/codecov/codecov-action)

If we want to add a badge or have nice reports on coverage, we could investigate CodeCov. It requires a token that should be added to the secrets of the repository: `CODECOV_TOKEN`.
