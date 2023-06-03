<div align="center">

  <h1>Monkey-lang Interpreter/Compiler</h1>
  <h5>Based on the awesome book "Writing An Interpreter In Go" by Thorsten Ball</h5>

![Work In Progress](https://img.shields.io/badge/Work%20In%20Progress-orange?style=for-the-badge)

</div>

## Directory structure

```bash
├── .gitignore  # ignores all language-specific stuff
├── go          # go implementation code
└── rust        # rust implementation code
```

### Rust

Run the following to test that the rust interpreter works

```rust
cd rust
cargo test
```

Run the following to run the REPL

```rust
cd rust // if not already in `rust` directory
cargo run src/main.rs
```

### Go

Run the following to test that the go interpreter works

```go
cd go
go test ./...
```

Run the following to run the REPL

```go
cd go // if not already in `go` directory
go run ./cmd/main.go
```
