module Sudoku where

import Data.Array

type Board = Array Int (Array Int Int)

makeBoard :: [[Int]] -> Board
makeBoard xs = listArray (0, length xs - 1) (map (\x -> listArray (0, length x - 1) x) xs)

boardToList :: Board -> [[Int]]
boardToList board = map elems (elems board)

get :: Board -> Int -> Int -> Int
get brd x y = brd ! x ! y

set :: Board -> Int -> Int -> Int -> Board
set brd x y val = let row = (brd ! x) // [(y, val)] in brd // [(x, row)]

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

isSolved :: Board -> Bool
isSolved brd = 0 `notElem` [ c | r <- elems brd, c <- elems r ]

solve :: Board -> Maybe Board
solve brd
  | isSolved brd = Just brd
  | otherwise = foldl (trySolve brd) Nothing
    [(x, y, z) | x <- [0..8], y <- [0..8], get brd x y == 0, z <- [1..9], possible brd x y z]

trySolve :: Board -> Maybe Board -> (Int, Int, Int) -> Maybe Board
trySolve brd Nothing (x, y, z) = solve (set brd x y z)
trySolve _ (Just b) _ = Just b