# Advent of Code 2023 — Haskell

Solutions for [Advent of Code 2023](https://adventofcode.com/2023) written in Haskell.

## Project Structure

```
.
├── aoc2023.cabal        # Build configuration
├── inputs/              # Puzzle inputs (one file per day)
│   └── day01.txt
├── src/                 # Solutions (one module per day)
│   └── Day01.hs
└── README.md
```

## Prerequisites

- **GHC** ≥ 9.4
- **Cabal** ≥ 3.8

Install via [GHCup](https://www.haskell.org/ghcup/).

## Running a Solution

```bash
# Build everything
cabal build all

# Run a specific day
cabal run day01
```

## Adding a New Day

1. Create `src/DayXX.hs` with a `Main` module.
2. Drop your input into `inputs/dayXX.txt`.
3. Add an executable stanza in `aoc2023.cabal`:

```cabal
executable dayXX
  import:  shared
  main-is: DayXX.hs
  hs-source-dirs: src
```

Then `cabal run dayXX`.
