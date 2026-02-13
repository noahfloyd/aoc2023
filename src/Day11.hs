{- |
  Advent of Code 2023 – Day 11: Cosmic Expansion

  Part 1: Empty rows/cols double in size. Sum Manhattan distances
          between all galaxy pairs.

  Part 2: Empty rows/cols expand by factor 1,000,000 instead of 2.
-}
module Main where

import Data.List (tails)

type Pos = (Int, Int)

-- ─── Parsing ─────────────────────────────────────────────────────

parseGalaxies :: String -> [Pos]
parseGalaxies input =
  [ (r, c)
  | (r, row) <- zip [0..] (lines input)
  , (c, ch)  <- zip [0..] row
  , ch == '#'
  ]

-- ─── Expansion ───────────────────────────────────────────────────

emptyRows :: String -> [Int]
emptyRows input =
  [ r | (r, row) <- zip [0..] (lines input), all (== '.') row ]

emptyCols :: String -> [Int]
emptyCols input =
  let lns  = lines input
      cols = [0 .. length (head lns) - 1]
  in  [ c | c <- cols, all (\row -> row !! c == '.') lns ]

-- Adjust galaxy coordinates given an expansion factor
expand :: Int -> [Int] -> [Int] -> [Pos] -> [Pos]
expand factor eRows eCols = map adjust
  where
    adjust (r, c) =
      let dr = length (filter (< r) eRows) * (factor - 1)
          dc = length (filter (< c) eCols) * (factor - 1)
      in  (r + dr, c + dc)

-- ─── Distance ────────────────────────────────────────────────────

manhattan :: Pos -> Pos -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

allPairDistances :: [Pos] -> Int
allPairDistances gs = sum [ manhattan a b | (a:rest) <- tails gs, b <- rest ]

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day11.txt"
  let galaxies = parseGalaxies input
      eRows    = emptyRows input
      eCols    = emptyCols input

  let part1 = allPairDistances (expand 2 eRows eCols galaxies)
  putStrLn $ "Part 1: " ++ show part1

  let part2 = allPairDistances (expand 1000000 eRows eCols galaxies)
  putStrLn $ "Part 2: " ++ show part2
