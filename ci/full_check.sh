#!/bin/bash
set -e
set -x

cargo fmt --all --check

cargo clippy --no-default-features --all-targets

cargo clippy --all-features --all-targets

cargo test --all-features --all-targets
cargo test --all-features --doc

cargo doc --no-deps --all-features