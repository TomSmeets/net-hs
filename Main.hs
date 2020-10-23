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
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Game = Game { game_state :: State
                 , game_rand  :: StdGen
                 , game_mouse :: Maybe Pos
                 , game_view  :: Viewport
                 }

data State = Gen [Gen.Grid] | Play (Play.Grid)

data Viewport = Viewport Float Float Float

vp_from_state (w, h) state = Viewport (min sa sb) dx dy
  where
    sa  = w / fromIntegral gw
    sb  = h / fromIntegral gh

    dx = - fromIntegral gw / 2 + 0.5
    dy = - fromIntegral gh / 2 + 0.5
    (Size gw gh) = game_state_size state

vp_project   (Viewport s dx dy)        = scale s s . translate dx dy
vp_unproject (Viewport s dx dy) (x, y) = Pos (round $ x/s - dx) (round $ y/s - dy)

game_state_size (Gen  ((Gen.Grid s _):_)) = s
game_state_size (Play (Play.Grid s _))    = s

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
                            Nothing -> error "nooo"
                 ) $ map (pos_wrap size . (`dir_add` pos)) dirs

state_update game@Game { game_state = Gen [x]    } = game { game_state = Play (gen2play x) }
state_update game@Game { game_state = Gen (x:xs) } = game { game_state = Gen xs }
state_update game@Game { game_state = Play g     } = game

solve_step r (Play.Grid (Size w h) m) = Play.Grid (Size w h) m2
  where
    (x, r2) = randomR (0, w-1) r
    (y, r3) = randomR (0, h-1) r2
    m2 = M.adjust Play.rot_left (Pos x y) m

state_display Game { game_state = (Gen  (x:xs)) } = Gen.grid_picture x
state_display game@Game { game_state = (Play s) } = pictures [ display_mouse game, Play.grid_picture s ]

display_mouse Game { game_mouse = Just (Pos x y) } = translate (fromIntegral x) (fromIntegral y) $ color (greyN 0.9) $ rectangleSolid 1 1
display_mouse _ = pictures []


event (EventResize (w, h)) g = g { game_view = vp_from_state (fromIntegral w, fromIntegral h) (game_state g) }
event (EventMotion (x, y)) g = g { game_mouse = Just $ vp_unproject (game_view g) (x, y) }

event (EventKey (Char 'r') Down _ _) game@Game { game_state = Play g } = let
    (r2, r3) = split (game_rand game)
  in game { game_rand = r3, game_state = Play (Play.grid_shuffle r2 g) }

event (EventKey (MouseButton dir) Down _ (mx, my)) game@(Game { game_rand=r, game_state = Play g }) = let
    (Play.Grid (Size w h) m) = g
    pos = vp_unproject (game_view game) (mx, my)
    do_rot = case dir of
        LeftButton  -> Play.rot_right
        RightButton -> Play.rot_left
        _           -> id
    m2 = M.adjust do_rot pos m
    (r2, r3) = split r
  in game { game_rand = r3, game_state = Play $ Play.Grid (Size w h) m2 }

event _ g = g

main = do
    gen <- newStdGen
    let s    = Size 6 6
    let grid = undupes $ Gen.grid_gen s gen
    gen <- newStdGen
    play (InWindow "hsnet" (800, 600) (0, 0)) white 60 (Game { game_mouse = Nothing, game_view = vp_from_state (800, 600) (Gen grid), game_rand = gen, game_state = Gen grid }) (\g -> vp_project (game_view g) $ state_display g) event (\_ g -> state_update g)
