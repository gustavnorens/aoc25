module Day6 (sol) where

import Data.Char (isSpace)
import Data.List (transpose)

sol :: IO ()
sol = do
  (ops : ss) <- reverse . lines <$> getContents
  print $ sum $ map (\(xs, op) -> p1 (trim op) (map (read . trim) xs)) $ parse ss ops
  print $ sum $ map p2 $ parse (reverse ss) ops

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

p1 :: String -> [Integer] -> Integer
p1 "*" = product
p1 "+" = sum
p1 _ = error "unexpected"

parse :: [String] -> String -> [([String], String)]
parse _ [] = []
parse [] _ = []
parse ss s = (map (take pos) ss, take pos s) : parse (map (drop pos) ss) (drop pos s)
  where
    pos = nextOp "*+" s

nextOp :: String -> String -> Int
nextOp ops s = go (tail s)
  where
    go :: String -> Int
    go [] = 1
    go (c : ss) = if c `elem` ops then 1 else 1 + go ss

p2 :: ([String], String) -> Integer
p2 (xs, op) =
  let fixed = map trim (transpose xs)
      pruned = filter (not . null) fixed
   in p1 (trim op) $ map read pruned