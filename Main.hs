module Main where

import qualified Gen
import qualified Play

import qualified Data.Map as M
import Data.Traversable
import Debug.Trace
import Data.Int (Int64)
import System.Random
import Graphics.Gloss
import Data.List

tailOrLast [x] = [x]
tailOrLast (x:xs) = xs

undupes [] = []
undupes [x] = [x]
undupes (x1:x2:xs) | x1 == x2  = undupes (x1:xs)
                   | otherwise = x1:undupes (x2:xs)

main = do
    gen <- newStdGen
    let s    = Gen.Size 20 20
    let grid = undupes $ Gen.grid_gen s gen
    simulate (InWindow "hsnet" (800, 600) (0, 0)) white 60 grid (scale 100 100 . Gen.grid_picture . head) (\_ _ g -> tailOrLast g)
