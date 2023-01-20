# SMPL Compiler

Hiya! This is (the beginnings of) a compiler for the `SMPL` language, developed as a part of CS242P.

## Running

This project is written in Rust.
Rust projects use `cargo` as their build tool.

### Install Rust

The easiest way is to use the install script from `rustup.rs`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Build and run

Build the project:

```bash
cargo build
```

Run the program:

```bash
cargo run -- -i "1+2 . 3/(4+5)"
```

Use the `RUST_LOG` environment variable to enable additional logging:

```bash
RUST_LOG=debug cargo run -- -i "1+2 . 3/(4+5)"
```