#!/bin/bash
set -e

echo cargo clippy --all-features --all-targets
cargo clippy --all-features --all-targets

echo cargo clippy --no-default-features --all-targets
cargo clippy --no-default-features --all-targets

echo cargo fmt --all --check
cargo fmt --all --check

echo cargo test
cargo test