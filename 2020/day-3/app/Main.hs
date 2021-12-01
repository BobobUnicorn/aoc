module Main where

import System.Environment (getArgs)

start = (0, 0)

delta = (3, 1)

deltas = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

atPositions :: (Int, Int) -> (Int, Int) -> [String] -> [Char]
atPositions (x, y) (dx, dy) lines =
  if y >= length lines
    then []
    else (lines !! y !! x) : atPositions ((x + dx) `mod` length (lines !! y), y + dy) (dx, dy) lines

countTrees :: [String] -> (Int, Int) -> Int
countTrees lines delta = length $ filter (== '#') $ atPositions start delta lines

answer :: [String] -> Int
answer lines = product $ map (countTrees lines) deltas

run :: Show b => ([String] -> b) -> IO ()
run fun = print . fun =<< fmap lines . readFile . head =<< getArgs

main :: IO ()
main = run answer
