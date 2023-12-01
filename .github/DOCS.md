# Github config and workflows

In this folder there is configuration for codecoverage, dependabot, and ci
workflows that check the library more deeply than the default configurations. This folder was merged <https://github.com/jonhoo/rust-ci-conf/> which provides a reasonably sensible base for writing your own ci on. 

An overview of the files in this project is available at:
<https://www.youtube.com/watch?v=xUH-4y92jPg&t=491s>

## Potential improvements

Taken from the template above, there are different workflows that wouldn't make sense for the `rune` project. I'll document them here in case we want to change our minds in the future and end up using them.

### cargo-hack
> `cargo-hack` checks combinations of feature flags to ensure that features are all additive which is required for feature unification.

[source](https://github.com/jonhoo/rust-ci-conf/blob/main/.github/workflows/check.yml#L77)

As we don't currently have any meaningful feature gates.
