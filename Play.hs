module Play where

import qualified Data.Map as M
import qualified Graphics.Gloss as G

import Common
import Data.Map (Map)
import Graphics.Gloss (Picture)
import System.Random

data Tile = Tile Bool Bool Bool Bool deriving (Eq, Ord, Show)
data Grid = Grid Size (Map Pos Tile)

mkTile :: [Bool] -> Tile
mkTile [a, b, c, d] = Tile a b c d

rot_left :: Tile -> Tile
rot_left  (Tile a b c d) = Tile b c d a

rot_right :: Tile -> Tile
rot_right (Tile a b c d) = Tile d a b c

tile_dirs :: Tile -> [Dir]
tile_dirs (Tile a b c d) = map snd $ filter fst $ zip [a, b, c, d] dirs

grid_picture :: Grid -> Picture
grid_picture (Grid s@(Size w h) m) = G.pictures $ map (\p@(Pos x y) -> G.translate (fromIntegral x) (fromIntegral y) $ tile_picture $ (m M.! p)) (size_points_inside s)

tile_picture :: Tile -> Picture
tile_picture t = G.pictures [ G.color (G.greyN 0.8) $ G.rectangleWire 1.0 1.0, G.color G.blue . G.pictures . map line . tile_dirs $ t ]
  where
    line (Dir x y) = G.line [(0, 0), (fromIntegral x / 2, fromIntegral y / 2)]


iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 f = id
iterateN n f = (iterateN (n-1) f) . f

grid_shuffle :: RandomGen g => g -> Grid -> Grid
grid_shuffle g (Grid s m) = Grid s $ M.fromList $ zipWith (\(k, v) r -> (k, iterateN r rot_left v)) (M.toList m) rs
  where
    rs = randomRs (0, 3) g
