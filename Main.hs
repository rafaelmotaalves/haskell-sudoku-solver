module Main where

import System.Environment
import IO
import Sudoku

main = do
    [filename] <- getArgs
    input <- readInput filename
    if (inputValidSize input) then
        let brd = makeBoard input in
            if (isValid brd) then
                let result = solve (makeBoard input) in
                    putStrLn (formatOutput result)
            else print "Invalid board: this sudoku is already invalid"
    else print "Invalid board: wrong size"

formatOutput :: Maybe Board -> String
formatOutput Nothing = "This board has no solution"
formatOutput (Just brd) = showSolution brd

showSolution :: Board -> String
showSolution brd = unlines (map join (boardToList brd))

join :: [Int] -> String
join [] = " "
join [x]  = show x
join (x:xs) = show x ++ " " ++ join xs