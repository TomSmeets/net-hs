module Common where

import qualified Data.Map as M
import System.Random

data Dir  = Dir  Int Int deriving (Show, Ord, Eq)
data Pos  = Pos  Int Int deriving (Show, Ord, Eq)
data Size = Size Int Int deriving (Show, Ord, Eq)

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

tailOrLast [x]    = [x]
tailOrLast (_:xs) = xs

undupes [] = []
undupes [x] = [x]
undupes (x1:x2:xs) | x1 == x2  = undupes (x1:xs)
                   | otherwise = x1:undupes (x2:xs)

---------------------------------------------------

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

size_points_inside :: Size -> [Pos]
size_points_inside (Size w h) = Pos <$> [0..(w-1)] <*> [0..(h-1)]
