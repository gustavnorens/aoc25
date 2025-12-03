module Day2 (sol) where

sol :: IO ()
sol = do
  pairs <- map parse . splitOn ',' <$> getContents
  let p1 = sum $ map (invalids invalid1) pairs
      p2 = sum $ map (invalids invalid2) pairs
   in print (p1, p2)

splitOn :: Char -> String -> [String]
splitOn c ss = chunk : if null rest then [] else splitOn c (tail rest)
  where
    (chunk, rest) = span (c /=) ss

parse :: String -> (Integer, Integer)
parse ss = case splitOn '-' ss of
  [s1, s2] -> (read s1, read s2)
  _ -> error "unexpected"

digits :: Integer -> Integer
digits i = truncate $ logBase (10 :: Double) (fromIntegral i) + 1

invalid1 :: Integer -> Bool
invalid1 i =
  let len = digits i
   in even len && (let half = div len 2 in div i (10 ^ half) == mod i (10 ^ half))

invalid2 :: Integer -> Bool
invalid2 i =
  let subs = parts s
      s = show i
   in any (`builds` s) subs

builds :: String -> String -> Bool
builds s match = elem match $ take (length match) (iterate (s ++) s)

parts :: String -> [String]
parts s = map (`take` s) [1 .. div (length s) 2]

invalids :: (Integer -> Bool) -> (Integer, Integer) -> Integer
invalids invalid (n, m) = sum $ filter invalid [n .. m]