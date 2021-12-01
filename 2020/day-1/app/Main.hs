module Main where

import Data.Functor ((<&>))
import Data.List (sort)
import System.Environment

getNumsThatSum :: Integer -> [Integer] -> [Integer] -> (Integer, Integer)
getNumsThatSum
  sum
  (sortedFirst : sortedRest)
  (reversedFirst : reversedRest)
    | sortedFirst + reversedFirst == sum = (sortedFirst, reversedFirst)
    | sortedFirst + reversedFirst < sum = getNumsThatSum sum sortedRest (reversedFirst : reversedRest)
    | otherwise = getNumsThatSum sum (sortedFirst : sortedRest) reversedRest
getNumsThatSum sum _ _ = (0, 0)

getThreeNumsThatSum :: Integer -> [Integer] -> Integer
getThreeNumsThatSum sum xs =
  head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == sum]

asIntegers :: String -> [Integer]
asIntegers = map read . lines

toIntegers :: String -> IO [Integer]
toIntegers = fmap asIntegers . readFile

mult :: (Integer, Integer) -> Integer
mult (x, y) = x * y

answer :: [Integer] -> Integer
-- answer xs = mult $ getNumsThatSum 2020 xs (reverse xs)
answer = getThreeNumsThatSum 2020

main :: IO ()
main = print =<< fmap (answer . sort . asIntegers) . readFile . head =<< getArgs
