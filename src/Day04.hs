{- |
  Advent of Code 2023 – Day 4: Scratchcards

  Part 1: Each card has winning numbers and your numbers.
          Points = 2^(matches-1) for matches > 0.  Sum all points.

  Part 2: Each card with N matches wins copies of the next N cards.
          Count the total number of scratchcards you end up with.
-}
module Main where

import qualified Data.Map.Strict as Map

-- ─── Parsing ─────────────────────────────────────────────────────

-- "Card  1: 41 48 83 | 83 86 6 31 17 9 48 53"
parseCard :: String -> ([Int], [Int])
parseCard s =
  let afterColon = tail (dropWhile (/= ':') s)
      (winPart, rest) = break (== '|') afterColon
      havePart  = tail rest   -- skip the '|'
  in  (map read (words winPart), map read (words havePart))

matches :: ([Int], [Int]) -> Int
matches (wins, have) = length (filter (`elem` wins) have)

-- ─── Part 1 ──────────────────────────────────────────────────────

points :: Int -> Int
points 0 = 0
points n = 2 ^ (n - 1)

-- ─── Part 2 ──────────────────────────────────────────────────────

-- Process cards left-to-right, tracking copies of each card.
countCards :: [Int] -> Int
countCards ms = sum (Map.elems finalCounts)
  where
    n = length ms
    initialCounts = Map.fromList [(i, 1) | i <- [0 .. n-1]]

    finalCounts = foldl processCard initialCounts [0 .. n-1]

    processCard counts i =
      let copies = counts Map.! i
          m      = ms !! i
          -- Add 'copies' to each of the next m cards
      in  foldl (\acc j -> Map.adjust (+ copies) j acc) counts
              [i+1 .. min (i+m) (n-1)]

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day04.txt"
  let cards = map parseCard (lines input)
      ms    = map matches cards

  let part1 = sum $ map points ms
  putStrLn $ "Part 1: " ++ show part1

  let part2 = countCards ms
  putStrLn $ "Part 2: " ++ show part2
