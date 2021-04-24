module Main where

import IO
import Sudoku

main = do
    input <- readInput "./input1.txt"
    let result = solve (makeBoard input) in
        print (formatOutput result)

formatOutput :: Maybe Board -> String
formatOutput Nothing = "This board has no solution"
formatOutput (Just brd) = show (boardToList brd)
