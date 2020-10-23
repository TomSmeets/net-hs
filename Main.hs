module Main where

import qualified Gen
import qualified Play
import qualified Data.Map as M

import Common
import Data.Int (Int64)
import Data.List
import Data.Traversable
import Debug.Trace
import Graphics.Gloss
import System.Random

data State = Gen StdGen [Gen.Grid] | Play StdGen (Play.Grid)

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

state_update (Gen r [x])    = Play r $ Play.grid_shuffle r $ gen2play x
state_update (Gen r (x:xs)) = Gen r xs
state_update s = s

state_display (Gen  _ (x:xs)) = Play.grid_picture $ gen2play x
state_display (Play _ x) = Play.grid_picture x

main = do
    gen <- newStdGen
    let s    = Size 20 20
    let grid = undupes $ Gen.grid_gen s gen
    gen <- newStdGen
    simulate (InWindow "hsnet" (800, 600) (0, 0)) white 60 (Gen gen grid) (scale 100 100 . state_display) (\_ _ g -> state_update g)
