# cobolparser

Parses COBOL source code into an [`AlephTree`](https://github.com/aleph-lang/aleph-syntax-tree).
Built with [LALRPOP](https://github.com/lalrpop/lalrpop).

## Installation

```toml
[dependencies]
cobolparser = "0.1"
```

## Usage

```rust
let ast = cobolparser::parse(source_code);
```

## Example

Input:

```cobol
PROCEDURE DIVISION.
    DISPLAY "Hello".
    STOP RUN.
```

Produces an `AlephTree::ProcedureDivision` containing `Display` and `Perform` nodes.

## Related

- [`aleph-syntax-tree`](https://github.com/aleph-lang/aleph-syntax-tree) — AST definition, includes COBOL-specific nodes
- [`alephc`](https://github.com/aleph-lang/aleph) — full compiler
