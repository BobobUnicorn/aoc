module Main where

import AocUtil (run)
import Data.Function ((&))
import Data.List (isSubsequenceOf, sort)
import Data.List.Extra (dropEnd, takeEnd)
import Data.List.Split (splitOn, splitOneOf)
import Data.Map (fromList, lookup)
import qualified Data.Set as S (fromList, member)
import Data.Tuple.Extra ((&&&))
import System.Environment (getArgs)
import Text.Regex.PCRE (RegexMaker (makeRegex), matchTest)
import Text.Regex.PCRE.String (Regex)
import Prelude hiding (lookup)

expectedFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

data PassportToken = PassportToken
  { name :: String,
    value :: String
  }

data Passport = Passport
  { byr :: String,
    iyr :: String,
    eyr :: String,
    hgt :: String,
    hcl :: String,
    ecl :: String,
    pid :: String
  }

fromTokenList :: [String] -> Maybe Passport
fromTokenList [byr, iyr, eyr, hgt, hcl, ecl, pid] = Just $ Passport byr iyr eyr hgt hcl ecl pid
fromTokenList _ = Nothing

fromTokens :: [PassportToken] -> Maybe Passport
fromTokens tokens =
  fromTokenList =<< allTokens
  where
    toTuple = name &&& value
    tokenMap = fromList $ map toTuple tokens
    getTokens = map lookup mandatoryFields
    allTokens = mapM (\getToken -> getToken tokenMap) getTokens

tokenize :: String -> PassportToken
tokenize str =
  case splitOn ":" str of
    [key, value] -> PassportToken key value
    _ -> PassportToken "" ""

tokenizePassport :: String -> [PassportToken]
tokenizePassport = map tokenize . splitOneOf " \n"

passports :: String -> [String]
passports = splitOn "\n\n"

hasAllFields :: [PassportToken] -> Bool
hasAllFields = isSubsequenceOf expectedFields . sort . map name

checkByr :: Passport -> Bool
checkByr = (\num -> num >= 1920 && num <= 2002) . (read :: String -> Int) . byr

checkIyr :: Passport -> Bool
checkIyr = (\num -> num >= 2010 && num <= 2020) . (read :: String -> Int) . iyr

checkEyr :: Passport -> Bool
checkEyr = (\num -> num >= 2020 && num <= 2030) . (read :: String -> Int) . eyr

checkHgt :: Passport -> Bool
checkHgt passport =
  case takeEnd 2 $ hgt passport of
    "cm" -> (\num -> num >= 150 && num <= 193) $ (read :: String -> Int) $ dropEnd 2 $ hgt passport
    "in" -> (\num -> num >= 59 && num <= 76) $ (read :: String -> Int) $ dropEnd 2 $ hgt passport
    _ -> False

colorRegex = (makeRegex :: String -> Regex) "^#[a-f0-9]{6}$"

checkHcl :: Passport -> Bool
checkHcl = matchTest colorRegex . hcl

eyeColours = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

checkEcl :: Passport -> Bool
checkEcl = flip S.member eyeColours . ecl

checkPid :: Passport -> Bool
checkPid = matchTest ((makeRegex :: String -> Regex) "^[0-9]{9}$") . pid

allChecks = [checkByr, checkIyr, checkEyr, checkHgt, checkHcl, checkEcl, checkPid]

validate :: [PassportToken] -> Bool
validate tokens =
  case fromTokens tokens of
    Just passport -> foldr (\next memo -> memo && next passport) True allChecks
    Nothing -> False

answer :: String -> Int
answer = length . filter (validate . tokenizePassport) . passports

main :: IO ()
main = run answer
