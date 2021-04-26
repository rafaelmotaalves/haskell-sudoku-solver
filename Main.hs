module Main where

import System.Environment
import IO
import Sudoku

main = do
    [filename] <- getArgs
    input <- readInput filename
    if inputValidSize input then
        let brd = makeBoard input in
            if isValid brd then
                let result = solve brd in putStrLn (formatOutput result)
            else print "Invalid board: this sudoku is already invalid."
    else print "Invalid board: wrong size, expected a 9x9 board."

formatOutput :: Maybe Board -> String
formatOutput Nothing = "Couldn't find a solution for the board."
formatOutput (Just brd) = showSolution brd

showSolution :: Board -> String
showSolution brd = unlines (map joinRow (boardToList brd))

joinRow :: [Int] -> String
joinRow [] = ""
joinRow [x]  = show x
joinRow (x:xs) = show x ++ " " ++ joinRow xs