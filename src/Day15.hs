{- |
  Advent of Code 2023 – Day 15: Lens Library

  Part 1: Sum of HASH values for each comma-separated step.

  Part 2: Process lens operations into 256 boxes.
          Compute total focusing power.
-}
module Main where

import Data.Char (ord)
import Data.List (findIndex)

-- ─── HASH algorithm ──────────────────────────────────────────────

hash :: String -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

-- ─── Parsing ─────────────────────────────────────────────────────

splitComma :: String -> [String]
splitComma [] = []
splitComma s  =
  let (w, rest) = break (== ',') s
  in  w : case rest of
            []     -> []
            (_:rs) -> splitComma rs

-- Remove trailing newline
strip :: String -> String
strip = filter (/= '\n')

-- ─── Part 2: Lens operations ────────────────────────────────────

type Lens = (String, Int)   -- (label, focal length)
type Boxes = [[Lens]]       -- 256 boxes

emptyBoxes :: Boxes
emptyBoxes = replicate 256 []

data Op = Remove String | Add String Int

parseOp :: String -> Op
parseOp s =
  case break (\c -> c == '-' || c == '=') s of
    (label, "-")      -> Remove label
    (label, '=':rest) -> Add label (read rest)
    _                 -> error $ "bad op: " ++ s

applyOp :: Boxes -> Op -> Boxes
applyOp boxes (Remove label) =
  let box = hash label
      lenses = boxes !! box
      lenses' = filter (\(l, _) -> l /= label) lenses
  in  replace box lenses' boxes

applyOp boxes (Add label fl) =
  let box = hash label
      lenses = boxes !! box
      lenses' = case findIndex (\(l, _) -> l == label) lenses of
                  Just i  -> take i lenses ++ [(label, fl)] ++ drop (i+1) lenses
                  Nothing -> lenses ++ [(label, fl)]
  in  replace box lenses' boxes

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- ─── Focusing power ─────────────────────────────────────────────

focusingPower :: Boxes -> Int
focusingPower boxes =
  sum [ (boxNum + 1) * slot * fl
      | (boxNum, lenses) <- zip [0..] boxes
      , (slot, (_, fl))  <- zip [1..] lenses
      ]

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day15.txt"
  let steps = splitComma (strip input)

  let part1 = sum $ map hash steps
  putStrLn $ "Part 1: " ++ show part1

  let ops   = map parseOp steps
      boxes = foldl applyOp emptyBoxes ops
      part2 = focusingPower boxes
  putStrLn $ "Part 2: " ++ show part2
