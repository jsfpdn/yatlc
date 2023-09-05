# yatlc: yet-another-toy-language compiler

> This is a repository for the yatl compiler, a project for a compiler course.
> yatlc is written in [Zig 0.11.0](https://ziglang.org) and uses LLVM.

**yatl is an imperative, statically typed, lexically scoped and memory-unsafe language.**
Sample code can be found in the [`/examples`](/examples) directory.

**yatlc is a single-pass compiler**. The recursive descent parser does not explicitely
build the abstract syntax tree but directly emits the LLVM IR.

## Building yatlc

To build the compiler, run `zig build`, emitting `./zig-out/bin/yatlc` binary.

## Running yatlc

Example programs can be found in the `examples/` folder. To compile yatl program,
just run `./zig-out/bin/yatlc path/to/file.ytl`.

To see help, run `./zig-out/bin/yatlc --help`.

## Testing `yatlc`

To run tests, run `zig test src/tests.zig`.

When testing an unexpected compiler error, it is helpful to build yatlc with `zig build -Doptimize=debug`
to emit more information during runtime when something happens.
