{- |
  Advent of Code 2023 – Day 2: Cube Conundrum

  Part 1: Which games are possible with 12 red, 13 green, 14 blue?
          Sum their IDs.

  Part 2: For each game, find the minimum cubes of each colour needed.
          Sum the "power" (product of min r, g, b) of every game.
-}
module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- ─── Types ───────────────────────────────────────────────────────

data CubeSet = CubeSet { red :: Int, green :: Int, blue :: Int }
  deriving Show

data Game = Game { gameId :: Int, sets :: [CubeSet] }
  deriving Show

-- ─── Parsing ─────────────────────────────────────────────────────

-- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parseLine :: String -> Game
parseLine s =
  let (hdr, rest) = break (== ':') s
      gid         = read (filter isDigit hdr) :: Int
      draws       = splitOn ';' (tail rest)          -- after ':'
      cubeSets    = map parseDraw draws
  in  Game gid cubeSets

parseDraw :: String -> CubeSet
parseDraw s =
  let entries = splitOn ',' s
  in  foldl addEntry (CubeSet 0 0 0) entries

addEntry :: CubeSet -> String -> CubeSet
addEntry cs entry =
  let ws  = words entry
      n   = read (head ws) :: Int
      col = ws !! 1
  in  case col of
        c | "red"   `isPrefixOf` c -> cs { red   = red cs   + n }
          | "green" `isPrefixOf` c -> cs { green = green cs + n }
          | "blue"  `isPrefixOf` c -> cs { blue  = blue cs  + n }
          | otherwise              -> cs

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s  =
  let (w, rest) = break (== c) s
  in  w : case rest of
            []     -> []
            (_:rs) -> splitOn c rs

-- ─── Part 1 ──────────────────────────────────────────────────────

possible :: Game -> Bool
possible g = all (\cs -> red cs <= 12 && green cs <= 13 && blue cs <= 14)
                 (sets g)

-- ─── Part 2 ──────────────────────────────────────────────────────

minCubes :: Game -> CubeSet
minCubes g = foldl combine (CubeSet 0 0 0) (sets g)
  where combine acc cs = CubeSet (max (red acc) (red cs))
                                 (max (green acc) (green cs))
                                 (max (blue acc) (blue cs))

power :: CubeSet -> Int
power cs = red cs * green cs * blue cs

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day02.txt"
  let games = map parseLine (lines input)

  let part1 = sum [ gameId g | g <- games, possible g ]
  putStrLn $ "Part 1: " ++ show part1

  let part2 = sum $ map (power . minCubes) games
  putStrLn $ "Part 2: " ++ show part2
