module AocUtil (run, runLines, runSingle) where

import System.Environment (getArgs)

runLines :: Show b => ([String] -> b) -> IO ()
runLines fun = print . fun =<< fmap lines . readFile . head =<< getArgs

run :: Show b => (String -> b) -> IO ()
run fun = print . fun =<< readFile . head =<< getArgs

runSingle :: Show b => (String -> b) -> String -> IO ()
runSingle fun arg = print $ fun arg
