module Main where

import System.Environment (getArgs)
import Text.Regex

parseLine :: String -> (Int, Int, Char, String)
parseLine line =
  case matched of
    Just strs ->
      (read $ head strs, read $ strs !! 1, head $ strs !! 2, strs !! 3)
    Nothing -> (0, 0, 'a', "a")
  where
    matched = matchRegex (mkRegex "^([[:digit:]]+)-([[:digit:]]+) ([[:alpha:]]): ([[:alpha:]]+)$") line

isValid :: String -> Bool
isValid line =
  count >= min && count <= max
  where
    (min, max, letter, password) = parseLine line
    count = length $ filter (letter ==) password

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

isValidPt2 :: String -> Bool
isValidPt2 line =
  xor (password !! (first - 1) == letter) (password !! (second - 1) == letter)
  where
    (first, second, letter, password) = parseLine line

countValid :: [String] -> Int
countValid = length . filter isValidPt2

asLines :: IO [String]
asLines = fmap lines . readFile . head =<< getArgs

main :: IO ()
main = print . countValid =<< asLines
