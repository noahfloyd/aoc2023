{- |
  Advent of Code 2023 – Day 3: Gear Ratios

  Part 1: Sum all numbers adjacent (incl. diagonally) to a symbol.
  Part 2: A gear is a '*' adjacent to exactly two part numbers.
          Sum the products (gear ratios) of all gears.
-}
module Main where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map

type Grid  = Map.Map (Int, Int) Char

-- ─── Build grid ──────────────────────────────────────────────────

buildGrid :: String -> Grid
buildGrid input =
  Map.fromList [ ((r, c), ch)
               | (r, row) <- zip [0..] (lines input)
               , (c, ch)  <- zip [0..] row
               ]

-- ─── Extract number spans ────────────────────────────────────────

-- A number span: (row, startCol, endCol, value)
type NumSpan = (Int, Int, Int, Int)

extractNumbers :: String -> [NumSpan]
extractNumbers input =
  concatMap extractRow (zip [0..] (lines input))
  where
    extractRow (r, row) = go r 0 row

    go _ _ [] = []
    go r c s@(ch:_)
      | isDigit ch =
          let digits = takeWhile isDigit s
              len    = length digits
              val    = read digits
          in  (r, c, c + len - 1, val) : go r (c + len) (drop len s)
      | otherwise  = go r (c + 1) (tail s)

-- ─── Adjacency ───────────────────────────────────────────────────

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

-- All 8-neighbour positions of a span
neighbours :: NumSpan -> [(Int, Int)]
neighbours (r, c0, c1, _) =
  [ (r + dr, c + dc)
  | c <- [c0 .. c1]
  , (dr, dc) <- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
  ]

-- ─── Part 1 ──────────────────────────────────────────────────────

isPartNumber :: Grid -> NumSpan -> Bool
isPartNumber grid ns =
  any (\pos -> maybe False isSymbol (Map.lookup pos grid)) (neighbours ns)

-- ─── Part 2 ──────────────────────────────────────────────────────

-- Build a map from every '*' position to the list of adjacent part numbers
gearMap :: Grid -> [NumSpan] -> Map.Map (Int, Int) [Int]
gearMap grid nums =
  foldl addNum Map.empty nums
  where
    addNum acc ns@(_, _, _, val) =
      let starNeighbours = filter (\p -> Map.lookup p grid == Just '*') (neighbours ns)
          -- deduplicate positions for this span
          uniqueStars = Map.keys (Map.fromList [(p, ()) | p <- starNeighbours])
      in  foldl (\m p -> Map.insertWith (++) p [val] m) acc uniqueStars

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day03.txt"
  let grid = buildGrid input
      nums = extractNumbers input

  let part1 = sum [ v | ns@(_, _, _, v) <- nums, isPartNumber grid ns ]
  putStrLn $ "Part 1: " ++ show part1

  let gm    = gearMap grid nums
      part2 = sum [ product vs | vs <- Map.elems gm, length vs == 2 ]
  putStrLn $ "Part 2: " ++ show part2
