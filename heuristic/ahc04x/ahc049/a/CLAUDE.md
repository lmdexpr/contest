# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an AtCoder Heuristic Contest (AHC049) solution codebase for the "Durability-Constrained Transport" problem. The project uses OCaml with the `dune` build system.

## Build and Development Commands

### Building
```bash
dune build
```

### Running the solution
```bash
dune exec ./a.exe < test/sample-1.in
```

### Testing with sample data
```bash
dune exec ./a.exe < test/sample-1.in > output.txt
diff output.txt test/sample-1.out
```

## Code Architecture

- **Main solution**: `a.ml` - Contains the main algorithm implementation
- **Build configuration**: `dune` - Defines the executable and required libraries
- **Test data**: `test/` directory contains sample input/output files
- **Contest metadata**: `contest.acc.json` - Contains contest and problem information

### Dependencies
The project uses several OCaml libraries as specified in the dune file:
- `core` - Jane Street's alternative standard library
- `iter` - Iterators library
- `num` - Arbitrary precision arithmetic
- `containers` - Extension of OCaml standard library
- `batteries` - Community-maintained standard library extension

### Input/Output Pattern
The solution follows a typical competitive programming pattern:
1. Read input using `scanf` from the `Core` library
2. Process the data to solve the optimization problem
3. Output the result using `printf`

## Contest Context

This is part of AHC049 (Toyota Programming Contest 2025#3), a heuristic optimization contest where solutions are evaluated based on score rather than correctness alone. The problem involves durability-constrained transport optimization.