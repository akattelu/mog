# mog

`mog` is a dynamically-typed, compiled programming language. The syntax is a superset of lua. It uses [QBE](https://c9x.me/compile/) as a backend for optimization and targetting different architetctures.

Mog is heavily WIP and primarily built for learning purposes. It is not intended to be used in production environments.

## Getting Started

Make sure you have `zig` installed and then compilation should be simple:
```sh
zig build                       # build the mog binary
zig build test --summary all    # run all tests
zig build -Drelease=true        # release for all supported targets
zig fmt --check .               # run formatter
```

The `vendor/qbe/` contains a macOS aarch64 binary for QBE. If you're working on a different architecture you'll need a variant of QBE which you can download [here](https://c9x.me/compile/releases.html)

## AI usage

When working on this application I mostly use claude for writing tests or doing minor refactors. The `CLAUDE.md` file contains helpful context for the codebase.

