{- |
  Advent of Code 2023 – Day 7: Camel Cards

  Part 1: Rank poker-like hands by type then card strength.
          Total winnings = sum of (rank * bid).

  Part 2: J is now a Joker (weakest card individually, but wild
          for determining hand type).  Re-rank and re-score.
-}
module Main where

import Data.List (sortBy, group, sort)
import Data.Ord  (comparing, Down(..))

-- ─── Types ───────────────────────────────────────────────────────

data HandBid = HandBid { hand :: String, bid :: Int } deriving Show

-- ─── Parsing ─────────────────────────────────────────────────────

parseLine :: String -> HandBid
parseLine s = let [h, b] = words s in HandBid h (read b)

-- ─── Hand type classification ────────────────────────────────────

-- Encode hand type as an Int (higher = better)
--   6 = five of a kind
--   5 = four of a kind
--   4 = full house
--   3 = three of a kind
--   2 = two pair
--   1 = one pair
--   0 = high card
handType :: String -> Int
handType h =
  let counts = sortBy (comparing Down) $ map length $ group $ sort h
  in  classifyCounts counts

classifyCounts :: [Int] -> Int
classifyCounts (5:_)   = 6
classifyCounts (4:_)   = 5
classifyCounts (3:2:_) = 4
classifyCounts (3:_)   = 3
classifyCounts (2:2:_) = 2
classifyCounts (2:_)   = 1
classifyCounts _       = 0

-- Part 2: J is wild.  Remove Js, add their count to the largest group.
handTypeJoker :: String -> Int
handTypeJoker h =
  let jokers   = length (filter (== 'J') h)
      rest     = filter (/= 'J') h
      counts   = sortBy (comparing Down) $ map length $ group $ sort rest
      adjusted = case counts of
                   []     -> [jokers]           -- all jokers → 5 of a kind
                   (c:cs) -> (c + jokers) : cs
  in  classifyCounts adjusted

-- ─── Card strength ──────────────────────────────────────────────

cardStrength :: Char -> Int
cardStrength 'A' = 14
cardStrength 'K' = 13
cardStrength 'Q' = 12
cardStrength 'J' = 11
cardStrength 'T' = 10
cardStrength c   = read [c]

cardStrengthJoker :: Char -> Int
cardStrengthJoker 'J' = 1    -- weakest
cardStrengthJoker c   = cardStrength c

-- ─── Comparison ──────────────────────────────────────────────────

compareHands :: (String -> Int) -> (Char -> Int) -> HandBid -> HandBid -> Ordering
compareHands typeFn strengthFn a b =
  compare (typeFn (hand a)) (typeFn (hand b))
    <> compare (map strengthFn (hand a)) (map strengthFn (hand b))

-- ─── Scoring ─────────────────────────────────────────────────────

totalWinnings :: [HandBid] -> (String -> Int) -> (Char -> Int) -> Int
totalWinnings hands typeFn strengthFn =
  let sorted = sortBy (compareHands typeFn strengthFn) hands
  in  sum $ zipWith (\rank hb -> rank * bid hb) [1..] sorted

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day07.txt"
  let hands = map parseLine (lines input)

  let part1 = totalWinnings hands handType cardStrength
  putStrLn $ "Part 1: " ++ show part1

  let part2 = totalWinnings hands handTypeJoker cardStrengthJoker
  putStrLn $ "Part 2: " ++ show part2
