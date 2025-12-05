module Day5 (sol) where

import Data.List (sort)

type Range = (Integer, Integer)

sol :: IO ()
sol =
  do
    (ranges, queries) <- parse <$> getContents
    print $ length $ filter (\n -> any (\(l, r) -> n >= l && n < r) ranges) queries
    print $ sum $ map (\(l, r) -> abs (l - r) + 1) $ shrink $ sort ranges

parse :: String -> ([(Integer, Integer)], [Integer])
parse ss = (map pr ranges, map read (tail queries))
  where
    (ranges, queries) = break null (lines ss)
    pr :: String -> (Integer, Integer)
    pr s = let (left, right) = span (/= '-') s in (read left, read (tail right))

combine :: Range -> Range -> Range
combine (l1, r1) (l2, r2) = let ln = min l1 l2 in let rn = max r1 r2 in (ln, rn)

overlaps :: Range -> Range -> Bool
overlaps (l1, r1) (l2, r2) = l1 <= r2 && r1 >= l2

shrink :: [Range] -> [Range]
shrink [] = []
shrink [r] = [r]
shrink (r1 : r2 : ranges) = if overlaps r1 r2 then shrink (combine r1 r2 : ranges) else r1 : shrink (r2 : ranges)