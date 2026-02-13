{- |
  Advent of Code 2023 – Day 8: Haunted Wasteland

  Part 1: Follow L/R instructions from AAA until you reach ZZZ.
          Count steps.

  Part 2: Start simultaneously from every node ending in 'A'.
          Continue until ALL current nodes end in 'Z' at the same step.
          Use LCM of individual cycle lengths.
-}
module Main where

import qualified Data.Map.Strict as Map

-- ─── Types ───────────────────────────────────────────────────────

type Network = Map.Map String (String, String)

-- ─── Parsing ─────────────────────────────────────────────────────

-- "AAA = (BBB, CCC)"
parseNode :: String -> (String, (String, String))
parseNode s =
  let name  = take 3 s
      left  = take 3 (drop 7 s)     -- after "AAA = ("
      right = take 3 (drop 12 s)    -- after "AAA = (BBB, "
  in  (name, (left, right))

parseInput :: String -> (String, Network)
parseInput input =
  let ls       = lines input
      instrs   = head ls
      nodeLines = drop 2 ls   -- skip blank line
      network   = Map.fromList (map parseNode nodeLines)
  in  (instrs, network)

-- ─── Navigation ──────────────────────────────────────────────────

step :: Network -> Char -> String -> String
step net dir node =
  let (l, r) = net Map.! node
  in  if dir == 'L' then l else r

-- Count steps from a start node until a predicate is satisfied
countSteps :: Network -> String -> (String -> Bool) -> [Char] -> Int
countSteps net start done instrs = go start (cycle instrs) 0
  where
    go node _      n | done node = n
    go node (d:ds) n = go (step net d node) ds (n + 1)
    go _    []     _ = error "impossible: cycle produces infinite list"

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day08.txt"
  let (instrs, network) = parseInput input

  -- Part 1
  let part1 = countSteps network "AAA" (== "ZZZ") instrs
  putStrLn $ "Part 1: " ++ show part1

  -- Part 2: LCM of all individual ghost cycles
  let starts = filter (\n -> last n == 'A') (Map.keys network)
      cycles = map (\s -> countSteps network s (\n -> last n == 'Z') instrs) starts
      part2  = foldl1 lcm cycles
  putStrLn $ "Part 2: " ++ show part2
