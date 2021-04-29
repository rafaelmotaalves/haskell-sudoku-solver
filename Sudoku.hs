module Sudoku where

import Data.Array
import Data.List
import Data.List.Split
import Control.Monad

type Board = Array Int Int

makeBoard :: [[Int]] -> Board
makeBoard xs = listArray (0, length (concat xs) - 1) (concat xs)

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
isValid brd = hasValidSize brd && isWellFormed brd

isWellFormed :: Board -> Bool
isWellFormed brd = all (\(x, y) -> let val = (get brd x y) in if (val /= 0) then (possible (set brd x y 0) x y val) else True) [ (x, y) | x <- [0..8], y <- [0..8]]

hasValidSize :: Board -> Bool
hasValidSize xs = length xs == 81

solve :: Board -> [Board]
solve brd = solve' brd emptys
  where emptys = [ (x, y) | x <- [0..8], y <- [0..8], get brd x y == 0]

candidates :: Board -> Int -> Int -> [Int]
candidates brd x y = [1..9] \\ (row++col++box)
  where
    row = rowValues brd x 
    col = colValues brd y
    box = boxValues brd x y 

rowValues brd x = [ v | i <- [0..8], let v = get brd x i, v /= 0]
colValues brd y = [ v | i <- [0..8], let v = get brd i y, v /= 0]

boxValues brd x y = [ v | i <- [base_x..base_x + 2], j <- [base_y..base_y + 2], let v = get brd i j, v /= 0]
  where base_x = (x `div` 3) * 3
        base_y = (y `div` 3) * 3

solve' :: Board -> [(Int, Int)] -> [Board]
solve' brd [] = [brd]
solve' brd ((i,j):xs) = case (candidates brd i j) of
  [] -> []
  cands -> concat [ solve' (set brd i j c) xs | c <- cands ]