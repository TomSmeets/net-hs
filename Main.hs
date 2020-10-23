module Main where

import qualified Gen
import qualified Play
import Common

import qualified Data.Map as M
import Data.Traversable
import Debug.Trace
import Data.Int (Int64)
import System.Random
import Graphics.Gloss
import Data.List

gen2play :: Gen.Grid -> Play.Grid
gen2play g@(Gen.Grid size m) = Play.Grid size $ M.fromList $ map (\p -> (p, gen2play_tile p g)) (size_points_inside size)

gen2play_tile :: Pos -> Gen.Grid -> Play.Tile
gen2play_tile pos (Gen.Grid size m) = Play.mkTile $ zipWith (||) self around
  where
    self  = case M.lookup pos m of
        Just d  -> map (== d)        dirs
        Nothing -> map (const False) dirs
    
    around :: [Bool]
    around = map (\p -> case M.lookup p m of
                            Just d -> pos_wrap size (dir_add d p) == pos
                            Nothing -> False
                 ) $ map (`dir_add` pos) dirs

main = do
    gen <- newStdGen
    let s    = Size 20 20
    let grid = undupes $ Gen.grid_gen s gen
    simulate (InWindow "hsnet" (800, 600) (0, 0)) white 60 grid (scale 100 100 . Play.grid_picture . Play.grid_shuffle . gen2play . head) (\_ _ g -> tailOrLast g)
