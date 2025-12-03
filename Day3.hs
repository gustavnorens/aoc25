module Day3 (sol) where

import Control.Monad (msum)
import Data.Maybe (fromJust)

sol :: IO ()
sol = do
  batteries <- map (map (\c -> (read :: String -> Integer) [c])) . lines <$> getContents
  let p1 = sum $ map (read . concatMap show . flip go 2) batteries
      p2 = sum $ map (read . concatMap show . flip go 12) batteries
   in (print :: (Integer, Integer) -> IO ()) (p1, p2)

go :: [Integer] -> Integer -> [Integer]
go _ 0 = []
go xs n = let (y : ys) = getNext xs n in y : go ys (n - 1)

getNext :: [Integer] -> Integer -> [Integer]
getNext xs n = fromJust $ msum $ map (\m -> next xs m n) [9, 8 .. 1]

next :: [Integer] -> Integer -> Integer -> Maybe [Integer]
next [] _ _ = Nothing
next (x : xs) num len = if x == num && (toInteger (length (x : xs)) >= len) then Just (x : xs) else next xs num len