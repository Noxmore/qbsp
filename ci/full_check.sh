#!/bin/bash
set -e
set -x

cargo fmt --all --check

cargo clippy --no-default-features --all-targets --workspace

cargo clippy --all-features --all-targets --workspace

cargo test --all-features --all-targets
cargo test --all-features --doc

cargo doc --no-deps --all-features