{- |
  Advent of Code 2023 – Day 6: Wait For It

  Part 1: Multiple races. For each race (time T, record D),
          count how many hold-times beat the record.
          Answer = product of all counts.

  Part 2: Concatenate all time digits into one number, same for distance.
          Count winning hold-times for this single large race.

  Math: holding for h ms in a race of T ms → distance = h*(T-h).
        We need h*(T-h) > D, i.e. h² - Th + D < 0.
        Roots: (T ± √(T²-4D)) / 2.  Count integers strictly between roots.
-}
module Main where

-- ─── Parsing ─────────────────────────────────────────────────────

parseNums :: String -> [Int]
parseNums = map read . tail . words   -- skip label ("Time:" / "Distance:")

-- ─── Solving ─────────────────────────────────────────────────────

-- Count integers h in (lo, hi) exclusive, where lo and hi are the
-- roots of h² - T*h + D = 0.
waysToWin :: Int -> Int -> Int
waysToWin t d
  | disc < 0  = 0
  | otherwise =
      let sqrtDisc = sqrt (fromIntegral disc :: Double)
          lo       = (fromIntegral t - sqrtDisc) / 2.0
          hi       = (fromIntegral t + sqrtDisc) / 2.0
          -- We need strictly greater, so nudge exact integer roots
          loInt    = floor lo + 1
          hiInt    = ceiling hi - 1
      in  max 0 (hiInt - loInt + 1)
  where
    disc = t * t - 4 * d

-- ─── Main ────────────────────────────────────────────────────────

main :: IO ()
main = do
  input <- readFile "inputs/day06.txt"
  let [timeLine, distLine] = lines input
      times = parseNums timeLine
      dists = parseNums distLine

  let part1 = product $ zipWith waysToWin times dists
  putStrLn $ "Part 1: " ++ show part1

  -- Part 2: concatenate digits
  let bigTime = read (concatMap show times) :: Int
      bigDist = read (concatMap show dists) :: Int
      part2   = waysToWin bigTime bigDist
  putStrLn $ "Part 2: " ++ show part2
