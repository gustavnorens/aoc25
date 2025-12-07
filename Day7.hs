module Day7 (sol) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V

type Matrix a = Vector (Vector a)

type Pos = (Int, Int)

sol :: IO ()
sol = do
  rows <- map (map (\c -> if c == 'S' then '|' else c)) . lines <$> getContents
  print $ process rows
  let m = V.fromList (map V.fromList rows) in print $ p2 Map.empty (0, fromJust (V.findIndex ('|' ==) (V.head m))) m

next :: String -> String -> Integer -> ([Integer], Integer)
next [] [] _ = ([], 0)
next (c1 : s1) (c2 : s2) i =
  let (repl, count) = next s1 s2 (i + 1)
   in case (c1, c2) of
        ('|', '^') -> (i - 1 : i + 1 : repl, count + 1)
        ('|', '.') -> (i : repl, count)
        ('.', _) -> (repl, count)
        ('^', '.') -> (repl, count)
        _ -> error "unexpected"
next _ _ _ = error "unexpected"

process :: [String] -> Integer
process [] = 0
process [_] = 0
process (s1 : s2 : ss) = let (repl, count) = next s1 s2 0 in process (foldr (replace '|') s2 repl : ss) + count

replace :: Char -> Integer -> String -> String
replace c 0 (_ : ss) = c : ss
replace _ _ [] = []
replace c n (c' : ss) = c' : replace c (n - 1) ss

(!) :: Matrix a -> Pos -> a
(!) m (i, j) = m V.! i V.! j

p2 :: Map Pos Integer -> (Int, Int) -> Matrix Char -> (Map Pos Integer, Integer)
p2 dict pos@(x, y) m
  | x == V.length m - 1 || y == 0 || y == V.length (V.head m) - 1 = (dict, 1)
  | Just i <- Map.lookup pos dict = (dict, i)
  | otherwise =
      case m ! (x + 1, y) of
        '^' ->
          let (dict1, a) = p2 dict (x + 1, y + 1) m
              (dict2, b) = p2 dict1 (x + 1, y - 1) m
              val = a + b
           in (Map.insert pos val dict2, val)
        '.' ->
          let (dict1, a) = p2 dict (x + 1, y) m
           in (Map.insert pos a dict1, a)
        _ -> error "unexpteced"