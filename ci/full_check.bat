cargo clippy --all-targets || exit /b 1

cargo fmt --all --check || exit /b 1

cargo test || exit /b 1