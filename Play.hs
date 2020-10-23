module Play where

data Tile = Tile Bool Bool Bool Bool deriving (Eq, Ord, Show)

rot_left  (Tile a b c d) = Tile b c d a
rot_right (Tile a b c d) = Tile d a b c
