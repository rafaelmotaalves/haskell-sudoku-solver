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

formatOutput :: [Board] -> String
formatOutput solutions = solutionStr ++ showNumberOfSolutions numberOfSolutions
    where numberOfSolutions = length solutions
          solutionStr = if (numberOfSolutions > 0) then (showSolution (head solutions)) else ""

showNumberOfSolutions :: Int -> String
showNumberOfSolutions numberOfSolutions = "\n Number of solutions: " ++ show numberOfSolutions 

showSolution :: Board -> String
showSolution brd = unlines (map joinRow (boardToList brd))

joinRow :: [Int] -> String
joinRow [] = ""
joinRow [x]  = show x
joinRow (x:xs) = show x ++ " " ++ joinRow xs