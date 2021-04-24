module Sudoku where

import Data.Array
import Data.List.Split

type Board = Array Int Int

inputValidSize :: [[Int]] -> Bool
inputValidSize xs = (length (concat xs)) == 81

makeBoard :: [[Int]] -> Board
makeBoard xs = listArray (0, 80) (concat xs)

boardToList :: Board -> [[Int]]
boardToList board = chunksOf 9 (elems board) 

get :: Board -> Int -> Int -> Int
get brd x y = brd ! (x * 9 + y)

set :: Board -> Int -> Int -> Int -> Board
set brd x y val = brd // [(x * 9 + y, val)]

possible :: Board -> Int -> Int -> Int -> Bool
possible brd x y val = checkRow brd x val && checkColumn brd y val && checkSquare brd x y val

checkRow :: Board -> Int -> Int -> Bool
checkRow brd x val = val `notElem` [ get brd x i | i <- [0..8]]

checkColumn :: Board -> Int -> Int -> Bool
checkColumn brd y val = val `notElem` ([get brd i y | i <- [0..8]])

checkSquare :: Board -> Int -> Int -> Int -> Bool
checkSquare brd x y val = val `notElem` [ get brd i j | i <- [base_x..base_x + 2], j <- [base_y..base_y + 2]]
        where base_x = (x `div` 3) * 3
              base_y = (y `div` 3) * 3

isValid :: Board -> Bool
isValid brd = all (\(x, y) -> let val = (get brd x y) in if (val /= 0) then (possible (set brd x y 0) x y val) else True) [ (x, y) | x <- [0..8], y <- [0..8]]

isSolved :: Board -> Bool
isSolved brd = 0 `notElem` (elems brd)

solve :: Board -> Maybe Board
solve brd
  | isSolved brd = Just brd
  | otherwise = foldl (trySolve brd) Nothing
    [(i, j, z) | i <- [0..8], j <- [0..8], get brd i j == 0, z <- [1..9], possible brd i j z] 

trySolve :: Board -> Maybe Board -> (Int, Int, Int) -> Maybe Board
trySolve brd Nothing (x, y, z) = solve (set brd x y z)
trySolve _ (Just b) _ = Just b