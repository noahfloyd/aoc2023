{- |
  Advent of Code 2023 – Day 1: Trebuchet?!

  Part 1: On each line, find the first and last numeric digit,
          combine them into a two-digit number, and sum all lines.

  Part 2: Digits can also be spelled out ("one", "two", …, "nine").
          Handle overlapping words (e.g. "eightwo" → 8 and 2).
-}
module Main where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

-- | All the spelled-out digit words and their numeric values.
wordDigits :: [(String, Int)]
wordDigits =
  [ ("one",   1), ("two",   2), ("three", 3)
  , ("four",  4), ("five",  5), ("six",   6)
  , ("seven", 7), ("eight", 8), ("nine",  9)
  ]

-- ─── Part 1 ──────────────────────────────────────────────────────

-- | Extract only the raw numeric digits from a string.
numericDigits :: String -> [Int]
numericDigits = map digitToInt . filter isDigit

-- | Calibration value for Part 1: first and last numeric digit.
calibrationValue1 :: String -> Int
calibrationValue1 line =
  let ds = numericDigits line
  in  case ds of
        []    -> 0
        _     -> head ds * 10 + last ds

-- ─── Part 2 ──────────────────────────────────────────────────────

-- | Walk through the string character by character.
--   At each position, check if a spelled-out word starts there,
--   or if the character itself is a digit.  This naturally handles
--   overlaps like "eightwo" because we advance by one character
--   regardless of match length.
extractDigits :: String -> [Int]
extractDigits [] = []
extractDigits s@(c:cs)
  | isDigit c = digitToInt c : extractDigits cs
  | otherwise =
      case matchWord s of
        Just d  -> d : extractDigits cs   -- advance by 1 for overlaps
        Nothing ->     extractDigits cs
  where
    matchWord str = lookup True
      [ (w `isPrefixOf` str, v) | (w, v) <- wordDigits ]

-- | Calibration value for Part 2.
calibrationValue2 :: String -> Int
calibrationValue2 line =
  let ds = extractDigits line
  in  case ds of
        []    -> 0
        _     -> head ds * 10 + last ds

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day01.txt"
  let lns = lines input

  let part1 = sum $ map calibrationValue1 lns
  putStrLn $ "Part 1: " ++ show part1

  let part2 = sum $ map calibrationValue2 lns
  putStrLn $ "Part 2: " ++ show part2
