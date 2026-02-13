{- |
  Advent of Code 2023 – Day 5: If You Give A Seed A Fertilizer

  Part 1: Map individual seed numbers through a chain of range maps.
          Find the lowest resulting location number.

  Part 2: Seed line is now pairs (start, length) describing ranges.
          Map ranges through the chain (splitting when they partially
          overlap a mapping rule) and find the lowest location.
-}
module Main where

import Data.List (foldl')

-- ─── Types ───────────────────────────────────────────────────────

-- A single mapping rule: (destStart, srcStart, rangeLen)
type Rule  = (Int, Int, Int)
-- A mapping stage is a list of rules
type Stage = [Rule]
-- A range: (start, length)
type Range = (Int, Int)

-- ─── Parsing ─────────────────────────────────────────────────────

parseInput :: String -> ([Int], [Stage])
parseInput input =
  let blocks = splitBlocks (lines input)
      seeds  = parseSeeds (head blocks)
      stages = map parseStage (tail blocks)
  in  (seeds, stages)

parseSeeds :: [String] -> [Int]
parseSeeds (s:_) = map read (words (drop 7 s))  -- drop "seeds: "
parseSeeds _     = []

parseStage :: [String] -> Stage
parseStage (_:rest) = map parseRule rest   -- skip header line
parseStage _        = []

parseRule :: String -> Rule
parseRule s = case map read (words s) of
  [d, src, len] -> (d, src, len)
  _             -> error "bad rule"

-- Split input lines on blank lines
splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks ls =
  let (block, rest) = break null ls
  in  block : splitBlocks (dropWhile null rest)

-- ─── Part 1: single values ──────────────────────────────────────

applyStage :: Stage -> Int -> Int
applyStage [] v = v
applyStage ((d, s, len):rs) v
  | v >= s && v < s + len = d + (v - s)
  | otherwise             = applyStage rs v

mapThrough :: [Stage] -> Int -> Int
mapThrough stages v = foldl' (flip applyStage) v stages

-- ─── Part 2: range splitting ────────────────────────────────────

-- Apply one rule to a range.
-- Returns: (mapped portions, unmapped remainders)
applyRuleToRange :: Rule -> Range -> ([Range], [Range])
applyRuleToRange (d, s, len) (rStart, rLen)
  | rEnd <= s || rStart >= s + len =
      -- No overlap
      ([], [(rStart, rLen)])
  | otherwise =
      let overlapStart = max rStart s
          overlapEnd   = min rEnd (s + len)
          mapped       = (d + overlapStart - s, overlapEnd - overlapStart)
          before       = if rStart < s
                         then [(rStart, s - rStart)]
                         else []
          after        = if rEnd > s + len
                         then [(s + len, rEnd - (s + len))]
                         else []
      in  ([mapped], before ++ after)
  where
    rEnd = rStart + rLen

-- Apply a whole stage to a list of ranges
applyStageToRanges :: Stage -> [Range] -> [Range]
applyStageToRanges rules ranges = go rules ranges []
  where
    go []     unmapped mapped = mapped ++ unmapped  -- unmapped pass through
    go (r:rs) unmapped mapped =
      let results    = map (applyRuleToRange r) unmapped
          newMapped  = concatMap fst results
          remaining  = concatMap snd results
      in  go rs remaining (mapped ++ newMapped)

mapRanges :: [Stage] -> [Range] -> [Range]
mapRanges stages ranges = foldl' (flip applyStageToRanges) ranges stages

seedsToRanges :: [Int] -> [Range]
seedsToRanges []         = []
seedsToRanges (s:l:rest) = (s, l) : seedsToRanges rest
seedsToRanges _          = error "odd number of seed values"

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day05.txt"
  let (seeds, stages) = parseInput input

  let part1 = minimum $ map (mapThrough stages) seeds
  putStrLn $ "Part 1: " ++ show part1

  let ranges = seedsToRanges seeds
      part2  = minimum $ map fst (mapRanges stages ranges)
  putStrLn $ "Part 2: " ++ show part2
