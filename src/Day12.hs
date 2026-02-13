{- |
  Advent of Code 2023 – Day 12: Hot Springs

  Part 1: Count valid arrangements of damaged/operational springs
          matching the given group pattern.

  Part 2: Unfold each row 5 times (pattern joined by '?', groups
          repeated 5x).  Use memoization (Map-based DP).
-}
module Main where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)

-- ─── Types ───────────────────────────────────────────────────────

type Memo = Map.Map (Int, Int, Int) Int

-- ─── Parsing ─────────────────────────────────────────────────────

parseLine :: String -> (String, [Int])
parseLine s =
  let (pat, rest) = break (== ' ') s
      groups      = map read (splitOn ',' (tail rest))
  in  (pat, groups)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s  =
  let (w, rest) = break (== c) s
  in  w : case rest of
            []     -> []
            (_:rs) -> splitOn c rs

-- ─── DP solver ───────────────────────────────────────────────────

-- State: (position in pattern, position in groups list, current run length)
-- Returns: (count, updated memo)
solve :: String -> [Int] -> Int
solve pat groups =
  let (result, _) = go 0 0 0 Map.empty
  in  result
  where
    n = length pat
    m = length groups
    patArr = pat         -- use indexing via (!!)
    grpArr = groups

    go :: Int -> Int -> Int -> Memo -> (Int, Memo)
    go pi gi run memo
      | Just v <- Map.lookup (pi, gi, run) memo = (v, memo)
      | pi == n =
          -- End of pattern: check we've matched all groups
          let val | run == 0 && gi == m           = 1  -- no active run, all groups matched
                  | gi == m - 1 && run == grpArr !! gi = 1  -- finish last group exactly
                  | otherwise                     = 0
              memo' = Map.insert (pi, gi, run) val memo
          in  (val, memo')
      | otherwise =
          let ch = patArr !! pi
              -- Option 1: treat as '.' (operational)
              (dotVal, memo1)
                | ch == '#' = (0, memo)  -- can't be dot
                | run == 0  = go (pi+1) gi 0 memo  -- not in a run, skip
                | gi < m && run == grpArr !! gi =
                    go (pi+1) (gi+1) 0 memo  -- end current group
                | otherwise = (0, memo)  -- run doesn't match any group

              -- Option 2: treat as '#' (damaged)
              (hashVal, memo2)
                | ch == '.' = (0, memo1)  -- can't be hash
                | gi < m && run < grpArr !! gi =
                    go (pi+1) gi (run+1) memo1  -- extend current run
                | gi >= m = (0, memo1)  -- no more groups to match
                | otherwise = (0, memo1)  -- run would exceed group size

              total = dotVal + hashVal
              memo3 = Map.insert (pi, gi, run) total memo2
          in  (total, memo3)

-- ─── Unfolding for Part 2 ───────────────────────────────────────

unfold :: (String, [Int]) -> (String, [Int])
unfold (pat, groups) =
  ( intercalate "?" (replicate 5 pat)
  , concat (replicate 5 groups)
  )

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day12.txt"
  let rows = map parseLine (lines input)

  let part1 = sum [ solve pat grp | (pat, grp) <- rows ]
  putStrLn $ "Part 1: " ++ show part1

  let part2 = sum [ solve pat grp | row <- rows, let (pat, grp) = unfold row ]
  putStrLn $ "Part 2: " ++ show part2
