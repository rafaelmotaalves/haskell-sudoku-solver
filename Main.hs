module Main where

import System.Environment
import IO
import Sudoku

main = do
    [filename] <- getArgs
    input <- readInput filename
    let brd = makeBoard input in
        if isValid brd then
            let result = solve brd in putStrLn (formatOutput result)
        else print "Invalid board: check size or formation"

formatOutput :: Answer -> String
formatOutput (Answer solution numberOfSolutions) = showSolution solution ++ showNumberOfSolutions numberOfSolutions

showNumberOfSolutions :: Int -> String
showNumberOfSolutions numberOfSolutions = "\n Number of solutions: " ++ show numberOfSolutions 

showSolution :: Board -> String
showSolution brd = unlines (map joinRow (boardToList brd))

joinRow :: [Int] -> String
joinRow [] = ""
joinRow [x]  = show x
joinRow (x:xs) = show x ++ " " ++ joinRow xs