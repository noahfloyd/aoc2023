{- |
  Advent of Code 2023 – Day 10: Pipe Maze

  Part 1: Find the main loop starting from 'S'.
          Answer = farthest point = loop length / 2.

  Part 2: Count tiles enclosed by the loop.
          Uses ray-casting (scan each row left-to-right, count
          vertical crossings to determine inside/outside).
-}
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type Pos   = (Int, Int)
type Grid  = Map.Map Pos Char

-- ─── Build grid ──────────────────────────────────────────────────

buildGrid :: String -> Grid
buildGrid input =
  Map.fromList [ ((r, c), ch)
               | (r, row) <- zip [0..] (lines input)
               , (c, ch)  <- zip [0..] row
               ]

findStart :: Grid -> Pos
findStart = fst . head . filter ((== 'S') . snd) . Map.toList

-- ─── Connectivity ────────────────────────────────────────────────

-- Which directions does a pipe connect to? (dr, dc)
connections :: Char -> [(Int, Int)]
connections '|' = [(-1,0), (1,0)]
connections '-' = [(0,-1), (0,1)]
connections 'L' = [(-1,0), (0,1)]
connections 'J' = [(-1,0), (0,-1)]
connections '7' = [(1,0),  (0,-1)]
connections 'F' = [(1,0),  (0,1)]
connections _   = []

-- Check if position p connects towards direction (dr,dc)
connectsTo :: Grid -> Pos -> (Int, Int) -> Bool
connectsTo grid (r, c) (dr, dc) =
  case Map.lookup (r + dr, c + dc) grid of
    Just ch -> (-dr, -dc) `elem` connections ch
    Nothing -> False

-- Determine what pipe 'S' actually is
inferS :: Grid -> Pos -> Char
inferS grid pos =
  let dirs = filter (connectsTo grid pos) [(-1,0),(1,0),(0,-1),(0,1)]
  in  case dirs of
        [(-1,0),(1,0)]  -> '|'
        [(0,-1),(0,1)]  -> '-'
        [(-1,0),(0,1)]  -> 'L'
        [(-1,0),(0,-1)] -> 'J'
        [(1,0),(0,-1)]  -> '7'
        [(1,0),(0,1)]   -> 'F'
        _               -> '.'   -- shouldn't happen

-- ─── Find the loop (BFS-like walk) ──────────────────────────────

findLoop :: Grid -> Pos -> Char -> [Pos]
findLoop grid start sCh = go [start] Set.empty
  where
    lookupCh p = let ch = grid Map.! p in if ch == 'S' then sCh else ch

    go [] visited = Set.toList visited
    go (p:queue) visited
      | Set.member p visited = go queue visited
      | otherwise =
          let ch    = lookupCh p
              (r,c) = p
              nexts = [ (r+dr, c+dc)
                      | (dr, dc) <- connections ch
                      , Map.member (r+dr, c+dc) grid
                      ]
          in  go (queue ++ nexts) (Set.insert p visited)

-- ─── Part 2: ray casting ────────────────────────────────────────

-- Scan each row left-to-right.  Track inside/outside by counting
-- how many times we cross a vertical boundary of the loop.
-- '|' always toggles.  'L...7' toggles.  'F...J' toggles.
-- 'L...J' does NOT toggle.  'F...7' does NOT toggle.
countEnclosed :: Grid -> Set.Set Pos -> Char -> Int
countEnclosed grid loopSet sCh =
  let rows = if Map.null grid then 0
             else maximum (map fst (Map.keys grid)) + 1
      cols = if Map.null grid then 0
             else maximum (map snd (Map.keys grid)) + 1
  in  sum [ countRow r cols | r <- [0 .. rows - 1] ]
  where
    lookupCh p = case Map.lookup p grid of
                   Just 'S' -> sCh
                   Just ch  -> ch
                   Nothing  -> '.'

    countRow r maxC = go 0 False Nothing 0
      where
        go c inside lastBend acc
          | c >= maxC = acc
          | Set.member (r, c) loopSet =
              let ch = lookupCh (r, c)
              in  case ch of
                    '|' -> go (c+1) (not inside) lastBend acc
                    '-' -> go (c+1) inside lastBend acc
                    'L' -> go (c+1) inside (Just 'L') acc
                    'F' -> go (c+1) inside (Just 'F') acc
                    'J' -> case lastBend of
                             Just 'L' -> go (c+1) inside Nothing acc       -- L-J: no toggle
                             Just 'F' -> go (c+1) (not inside) Nothing acc -- F-J: toggle
                             _        -> go (c+1) inside Nothing acc
                    '7' -> case lastBend of
                             Just 'L' -> go (c+1) (not inside) Nothing acc -- L-7: toggle
                             Just 'F' -> go (c+1) inside Nothing acc       -- F-7: no toggle
                             _        -> go (c+1) inside Nothing acc
                    _   -> go (c+1) inside lastBend acc
          | inside    = go (c+1) inside lastBend (acc + 1)
          | otherwise = go (c+1) inside lastBend acc

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day10.txt"
  let grid  = buildGrid input
      start = findStart grid
      sCh   = inferS grid start
      loop  = findLoop grid start sCh

  let part1 = length loop `div` 2
  putStrLn $ "Part 1: " ++ show part1

  let loopSet = Set.fromList loop
      part2   = countEnclosed grid loopSet sCh
  putStrLn $ "Part 2: " ++ show part2
