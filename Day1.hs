module Day1 (sol) where

import Prelude hiding (Left, Right)

sol :: IO ()
sol = do
  ops <- reverse . map parse . lines <$> getContents
  let p1 = snd $ foldr go1 (50, 0) ops
      p2 = snd $ foldr go2 (50, 0) ops
   in print (p1, p2)

go1 :: Integer -> (Integer, Integer) -> (Integer, Integer)
go1 delta (analog, count) = let new = mod (analog + delta) 100 in (new, count + if new == 0 then 1 else 0)

parse :: String -> Integer
parse (ch : ss) = let op = if ch == 'L' then negate else id in op (read ss)
parse [] = error "unexpected"

go2 :: Integer -> (Integer, Integer) -> (Integer, Integer)
go2 delta (analog, count) =
  let new = analog + delta
      crossings
        | delta > 0 = div new 100
        | delta < 0 = div (analog - 1) 100 - div (new - 1) 100
        | otherwise = 0
   in (mod new 100, count + crossings)
