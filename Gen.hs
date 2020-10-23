module Gen where

import qualified Data.Map as M
import Data.Traversable
import Debug.Trace
import Data.Int (Int64)
import System.Random
import Graphics.Gloss
import Data.List

nth :: Int -> [a] -> a
nth _ [x]      = x
nth 0 (x:_)    = x
nth n (_:xs)   = nth (n - 1) xs

map_try_insert :: (Ord k) => k -> v -> M.Map k v -> Maybe (M.Map k v)
map_try_insert k v m = case M.member k m of
    True -> Nothing
    False -> Just $ M.insert k v m

shuffle :: RandomGen g => [a] -> g -> [a]
shuffle xs g = shuffleL (length xs) xs g
  where
    remove _ []     = (undefined, [])
    remove 0 (x:xs) = (x, xs)
    remove i (x:xs) = let (y, ys) = remove (i-1) xs in (y, x:ys)

    shuffleL 0 []  _ = []
    shuffleL l xs g1 = y : shuffleL (l-1) ys g2
      where
        (idx, g2) = randomR (0, (l-1)) g1
        (y,   ys) = remove idx xs

----------------------------

data Tile = Tile Pos Dir
data Grid = Grid Size (M.Map Pos Dir) deriving (Eq, Show)

data Dir  = Dir  Int Int deriving (Show, Ord, Eq)
data Pos  = Pos  Int Int deriving (Show, Ord, Eq)
data Size = Size Int Int deriving (Show, Ord, Eq)

dir_inv :: Dir -> Dir
dir_inv (Dir dx dy) = Dir (-dx) (-dy)

dir_add :: Dir -> Pos -> Pos
dir_add (Dir dx dy) (Pos x y) = Pos (dx + x) (dy + y)

dirs :: [Dir]
dirs = [ Dir 1 0, Dir 0 1, Dir (-1) 0, Dir 0 (-1) ]

dir_to_char :: Dir -> Char
dir_to_char (Dir ( 1) ( 0)) = '>'
dir_to_char (Dir ( 0) ( 1)) = 'v'
dir_to_char (Dir (-1) ( 0)) = '<'
dir_to_char (Dir ( 0) (-1)) = '^'
dir_to_char (Dir ( 0) ( 0)) = '+'
dir_to_char (Dir _    _)    = '?'

---------------------------------------------------

start_pos :: Size -> Pos
start_pos (Size w h) = Pos (w `div` 2) (h `div` 2)

pos_wrap :: Size -> Pos -> Pos
pos_wrap (Size w h) (Pos x y) = Pos (x `mod` w) (y `mod` h)

pos_around :: Size -> Pos -> [Tile]
pos_around s p = zipWith Tile (map (pos_wrap s . (`dir_add` p)) dirs)
                              (map dir_inv dirs)


---------------------------------------------------

grid_empty :: Size -> Grid
grid_empty s = Grid s (M.empty)

grid_gen_try_place :: Tile -> Grid -> Maybe Grid
grid_gen_try_place (Tile pos dir) (Grid size grid) = Grid size <$> map_try_insert pos dir grid

grid_gen_step :: RandomGen g => Tile -> Grid -> g -> ([Tile], Grid)
grid_gen_step (Tile pos dir) grid g_shuf = case grid_gen_try_place (Tile pos dir) grid of
    Nothing                 -> ([], grid)
    Just grid@(Grid size _) -> (shuffle (pos_around size pos) g_shuf, grid)

grid_gen_step_more :: RandomGen g => [Tile] -> Grid -> g -> [Grid]
grid_gen_step_more []     g _   = g : []
grid_gen_step_more (q:qs) g rnd = g : grid_gen_step_more (mix qs2 qs) g2 rnd2
  where
    mix []     ys = ys
    mix (x:y:xs) ys = x : y : (ys ++ xs)
    mix (x:xs) ys = x : (ys ++ xs)

    (rnd1, rnd2)  = split rnd
    (qs2, g2) = grid_gen_step q g rnd1

grid_gen s g = grid_gen_step_more [Tile (start_pos s) (Dir 0 0)] (grid_empty s) g

grid_show :: Grid -> String
grid_show (Grid (Size w h) m) = unlines $ map (\y -> concat $ map (\x -> check (Pos x y) ) [0..(w-1)]) [0..(h-1)]
  where
    check p@(Pos x y) = case M.lookup p m of
        Just d  -> (dir_to_char d : " ")
        Nothing -> ". "

grid_picture :: Grid -> Picture
grid_picture (Grid (Size w h) m) = translate (- fromIntegral w / 2) (- fromIntegral h / 2) $ pictures $ map (grid_picture_tile . uncurry Tile) (M.toList m)

grid_picture_tile (Tile (Pos x y) dir)
    = translate (fi x) (fi y)
    $ pictures [ rectangleWire 0.2 0.2
               , arr dir
               ]
  where
    arr (Dir x y) = line [(0, 0), (fi x, fi y)]
    fi = fromIntegral

---------------------------------------
