#!/bin/bash
set -e

echo cargo clippy --all-targets
cargo clippy --all-targets

echo cargo fmt --all --check
cargo fmt --all --check

echo cargo test
cargo test