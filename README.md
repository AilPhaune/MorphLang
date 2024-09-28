# Morph
Morph, a functional and imperative programming language.

## Implementation status

- [ ] Parser
- [ ] Semantic Analysis
- [ ] Type Checker
    - [ ] Type checking
    - [ ] Type inference
- [ ] LLVM IR Generation
- [ ] Build the binary

## Example builds

### See the parser combinators in action:
```sh
cargo run --example combinators
```

### Run the compiler
```sh
cargo run --bin morphc <input_file>
```