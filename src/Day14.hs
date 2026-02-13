{- |
  Advent of Code 2023 – Day 14: Parabolic Reflector Dish

  Part 1: Tilt north, compute load on north support beams.

  Part 2: Run 1 billion spin cycles (N, W, S, E).
          Use cycle detection to skip ahead.
-}
module Main where

import qualified Data.Map.Strict as Map

type Grid = [[Char]]

-- ─── Tilt north ──────────────────────────────────────────────────

-- Transpose, tilt each row left (= tilt north), transpose back
tiltNorth :: Grid -> Grid
tiltNorth = transpose . map tiltRowLeft . transpose

tiltSouth :: Grid -> Grid
tiltSouth = transpose . map tiltRowRight . transpose

tiltWest :: Grid -> Grid
tiltWest = map tiltRowLeft

tiltEast :: Grid -> Grid
tiltEast = map tiltRowRight

-- Tilt a single row so 'O' slides left (toward index 0)
tiltRowLeft :: String -> String
tiltRowLeft = concatMap slideLeft . splitOnCubes

-- Tilt a single row so 'O' slides right
tiltRowRight :: String -> String
tiltRowRight = concatMap slideRight . splitOnCubes

-- Split a row by '#' keeping '#' in the result
splitOnCubes :: String -> [String]
splitOnCubes [] = []
splitOnCubes s  =
  let (seg, rest) = break (== '#') s
  in  case rest of
        []       -> [seg]
        ('#':rs) -> seg : "#" : splitOnCubes rs
        _        -> [seg]  -- shouldn't happen

slideLeft :: String -> String
slideLeft s
  | s == "#"  = "#"
  | otherwise = let os = length (filter (== 'O') s)
                    ds = length s - os
                in  replicate os 'O' ++ replicate ds '.'

slideRight :: String -> String
slideRight s
  | s == "#"  = "#"
  | otherwise = let os = length (filter (== 'O') s)
                    ds = length s - os
                in  replicate ds '.' ++ replicate os 'O'

-- ─── Spin cycle ──────────────────────────────────────────────────

spinCycle :: Grid -> Grid
spinCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

-- ─── Load calculation ────────────────────────────────────────────

totalLoad :: Grid -> Int
totalLoad grid =
  let n = length grid
  in  sum [ n - r | (r, row) <- zip [0..] grid, c <- row, c == 'O' ]

-- ─── Cycle detection ────────────────────────────────────────────

findCycle :: Grid -> (Int, Int, Grid)  -- (cycleStart, cycleLen, grid at target)
findCycle grid = go grid Map.empty 0
  where
    target = 1000000000
    go g seen i =
      case Map.lookup g seen of
        Just start ->
          let cycleLen  = i - start
              remaining = (target - start) `mod` cycleLen
              final     = iterate spinCycle g !! remaining
          in  (start, cycleLen, final)
        Nothing ->
          go (spinCycle g) (Map.insert g i seen) (i + 1)

-- ─── Transpose ───────────────────────────────────────────────────

transpose :: [[a]] -> [[a]]
transpose []       = []
transpose ([] : _) = []
transpose xss      = map head xss : transpose (map tail xss)

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day14.txt"
  let grid = lines input

  let part1 = totalLoad (tiltNorth grid)
  putStrLn $ "Part 1: " ++ show part1

  let (_, _, finalGrid) = findCycle grid
      part2 = totalLoad finalGrid
  putStrLn $ "Part 2: " ++ show part2
