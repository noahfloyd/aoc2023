{- |
  Advent of Code 2023 – Day 9: Mirage Maintenance

  Part 1: For each history (list of ints), extrapolate the next value
          by repeatedly taking differences until all zero, then
          propagating back up.  Sum all extrapolated next values.

  Part 2: Extrapolate backwards (the previous value).
          Trick: just reverse each history and reuse the same logic.
-}
module Main where

-- ─── Core logic ──────────────────────────────────────────────────

differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs

-- Extrapolate the next value
extrapolateNext :: [Int] -> Int
extrapolateNext xs
  | all (== 0) xs = 0
  | otherwise     = last xs + extrapolateNext (differences xs)

-- Extrapolate the previous value = extrapolate next of the reversed list
extrapolatePrev :: [Int] -> Int
extrapolatePrev = extrapolateNext . reverse

-- ─── Parsing ─────────────────────────────────────────────────────

parseLine :: String -> [Int]
parseLine = map read . words

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day09.txt"
  let histories = map parseLine (lines input)

  let part1 = sum $ map extrapolateNext histories
  putStrLn $ "Part 1: " ++ show part1

  let part2 = sum $ map extrapolatePrev histories
  putStrLn $ "Part 2: " ++ show part2
