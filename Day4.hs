module Day4 (sol) where

import Data.Vector (Vector)
import qualified Data.Vector as V

type Matrix a = Vector (Vector a)

type Pos = (Int, Int)

sol :: IO ()
sol = do
  matrix <- V.fromList . map V.fromList . lines <$> getContents
  let 
    p1 = length $ accessible matrix
    p2 = totalAccessible matrix
   in print $ (p1, p2)

(!) :: Matrix a -> Pos -> a
(!) m (i, j) = m V.! i V.! j

surrounding :: Int -> Int -> Pos -> Int -> [Pos]
surrounding height width (x, y) i =
  [ (x + dx, y + dy)
    | dx <- [-i .. i],
      x + dx >= 0,
      x + dx < height,
      dy <- [-i .. i],
      y + dy >= 0,
      y + dy < width
  ]

cords :: Matrix a -> [Pos]
cords m = [(x, y) | x <- [0 .. length m - 1], y <- [0 .. length (m V.! 0) - 1]]

movable :: Matrix Char -> Pos -> Int -> Bool
movable m p size = (m ! p == '@') && 4 >= length (filter ('@' ==) (map (m !) neighbors))
  where
    neighbors = surrounding (length m) (length (m V.! 0)) p size


accessible :: Matrix Char -> [Pos]
accessible m = filter (flip (movable m) 1) (cords m)

update :: Matrix Char -> [Pos] -> Matrix Char
update = foldr (\(x, y) acc -> acc V.// [(x, (acc V.! x) V.// [(y, '.')])])

totalAccessible :: Matrix Char -> Int
totalAccessible m =
  let removed = accessible m
      new = update m removed
      count = length removed
   in count + if new == m then 0 else totalAccessible new