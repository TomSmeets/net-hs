module Main where

import qualified Gen
import qualified Play
import Gen (Size(Size), Pos(Pos), Dir(Dir), dir_add, pos_wrap)

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


grid_points (Size w h) = Pos <$> [0..(w-1)] <*> [0..(h-1)]

gen2play :: Gen.Grid -> Play.Grid
gen2play g@(Gen.Grid size m) = Play.Grid size $ M.fromList $ map (\p -> (p, gen2play_tile p g)) (grid_points size)

gen2play_tile :: Pos -> Gen.Grid -> Play.Tile
gen2play_tile pos (Gen.Grid size m) = Play.mkTile $ zipWith (||) self around
  where
    self  = case M.lookup pos m of
        Just d  -> map (== d)        Gen.dirs
        Nothing -> map (const False) Gen.dirs
    
    around :: [Bool]
    around = map (\p -> case M.lookup p m of
                            Just d -> pos_wrap size (dir_add d p) == pos
                            Nothing -> False
                 ) $ map (`dir_add` pos) Gen.dirs

main = do
    gen <- newStdGen
    let s    = Gen.Size 20 20
    let grid = undupes $ Gen.grid_gen s gen
    simulate (InWindow "hsnet" (800, 600) (0, 0)) white 60 grid (scale 100 100 . Play.grid_picture . Play.grid_shuffle . gen2play . head) (\_ _ g -> tailOrLast g)
