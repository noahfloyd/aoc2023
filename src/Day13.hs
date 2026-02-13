{- |
  Advent of Code 2023 – Day 13: Point of Incidence

  Part 1: Find the line of reflection (horizontal or vertical)
          in each pattern.  Score = cols_left + 100*rows_above.

  Part 2: Each pattern has exactly one smudge.  Find the reflection
          line where exactly 1 character differs across the mirror.
-}
module Main where

-- ─── Parsing ─────────────────────────────────────────────────────

-- Split input into blocks separated by blank lines
splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks ls =
  let (block, rest) = break null ls
  in  block : splitBlocks (dropWhile null rest)

-- ─── Reflection finding ─────────────────────────────────────────

-- Count total character differences across a candidate reflection axis.
-- For horizontal: compare row i vs row (2*axis - 1 - i)
-- 'axis' is the number of rows/cols before the mirror line.
reflectionDiffs :: [String] -> Int -> Int
reflectionDiffs rows axis =
  sum [ if rows !! i /= rows !! j then rowDiffs (rows !! i) (rows !! j) else 0
      | k <- [0 .. axis - 1]
      , let i = axis - 1 - k
            j = axis + k
      , j < length rows
      ]
  where
    rowDiffs a b = length (filter id (zipWith (/=) a b))

-- Find a reflection axis with exactly 'target' total diffs
findReflection :: Int -> [String] -> Int
findReflection target rows =
  let n = length rows
      candidates = [ ax | ax <- [1 .. n-1], reflectionDiffs rows ax == target ]
  in  case candidates of
        (x:_) -> x
        []    -> 0

-- Transpose pattern to check vertical reflections as horizontal
transpose :: [String] -> [String]
transpose []       = []
transpose ([] : _) = []
transpose xss      = map head xss : transpose (map tail xss)

-- Score a single pattern
scorePattern :: Int -> [String] -> Int
scorePattern target pat =
  let hRefl = findReflection target pat
      vRefl = findReflection target (transpose pat)
  in  100 * hRefl + vRefl

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day13.txt"
  let patterns = splitBlocks (lines input)

  let part1 = sum $ map (scorePattern 0) patterns
  putStrLn $ "Part 1: " ++ show part1

  let part2 = sum $ map (scorePattern 1) patterns
  putStrLn $ "Part 2: " ++ show part2
