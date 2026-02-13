{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_aoc2023 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "aoc2023"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Advent of Code 2023 solutions in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
