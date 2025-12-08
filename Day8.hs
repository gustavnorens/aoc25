{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use elemIndex" #-}

module Day8 (sol) where

import Data.List (findIndex, nub, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Pos = (Integer, Integer, Integer)

sol :: IO ()
sol = do
  positions <- map parse . lines <$> getContents
  let sorted = sortBy (\pos1 pos2 -> compare (uncurry distance pos1) (uncurry distance pos2)) (pair positions)
   in let mappings = p1 1000 (length positions) (Map.fromList (map (\pos -> (pos, Set.singleton pos)) positions)) sorted
       in do
            print $ product $ take 3 $ sortBy (flip compare) $ map Set.size $ nub $ Map.elems mappings
            print $ p1 (-1) (length positions) (Map.fromList (map (\pos -> (pos, Set.singleton pos)) positions)) sorted

parse :: String -> Pos
parse s =
  let (fst, snd') = splitAt (fromJust $ findIndex (== ',') s) s
   in let (snd, trd) = splitAt (fromJust $ findIndex (== ',') (tail snd')) (tail snd')
       in (read fst, read snd, read (tail trd))

pair :: (Ord a) => [a] -> [(a, a)]
pair xs = [(x, y) | x <- xs, y <- xs, x > y]

distance :: Pos -> Pos -> Integer
distance (x1, y1, z1) (x2, y2, z2) =
  let (dx, dy, dz) = (abs (x1 - x2), abs (y1 - y2), abs (z1 - z2))
   in dx * dx + dy * dy + dz * dz

getx :: Pos -> Integer
getx (x, _, _) = x

p1 :: Int -> Int -> Map Pos (Set Pos) -> [(Pos, Pos)] -> Map Pos (Set Pos)
p1 depth _ m [] = m
p1 0 _ m _ = m
p1 depth i m ((pos1, pos2) : xs) =
  let newset = Map.findWithDefault Set.empty pos1 m <> Map.findWithDefault Set.empty pos2 m
   in if Set.size newset == i then error $ show (getx pos1 * getx pos2) else p1 (depth - 1) i (foldr (`Map.insert` newset) m (Set.toList newset)) xs
