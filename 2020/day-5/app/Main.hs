module Main where

import AocUtil (runLines, runSingle)

binaryPartition :: String -> Int -> Int -> Char -> Char -> Int
binaryPartition (next : rest) min max lower upper
  | next == lower = binaryPartition rest min (floor $ (fromIntegral max + fromIntegral min) / 2) lower upper
  | next == upper = binaryPartition rest (ceiling $ (fromIntegral max + fromIntegral min) / 2) max lower upper
  | otherwise = 0
binaryPartition [] min _ _ _ = min

lineId :: String -> Int
lineId xs = row * 8 + column
  where
    row = binaryPartition (take 7 xs) 0 127 'F' 'B'
    column = binaryPartition (drop 7 xs) 0 7 'L' 'R'

answer :: [String] -> Int
answer = maximum . map lineId

answer2 :: [String] -> Int
answer2 xs = head [id | id <- allIds, (id + 1) `elem` ids, (id - 1) `elem` ids, id `notElem` ids]
  where
    ids = map lineId xs
    allIds = [(minimum ids) .. (maximum ids)]

main :: IO ()
main = runLines answer2
