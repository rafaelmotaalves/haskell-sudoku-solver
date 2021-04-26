# Haskell Sudoku Solver
This is a Haskell implementation of the Sudoku Solver algorithm using Backtrack.

## Building
To install the dependencies execute:
```
cabal install
```
Then execute the build script:
```
cabal build
```

## Executing
Execute the binary passing the txt file with the input sudoku board:
```
./dist/build/sudoku-solver/sudoku-solver <file-name>
```
The format for the input file is:
```
0 8 0 9 0 1 6 0 0
9 6 0 3 4 0 0 0 0
0 5 1 0 0 6 0 0 3
5 4 8 0 3 0 0 0 6
0 0 0 0 6 0 1 0 0
0 0 6 0 9 8 2 0 5
0 0 0 6 8 0 5 0 4
8 0 4 0 5 0 7 6 0
0 0 0 0 0 7 3 0 0
```