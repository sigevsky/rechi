module Utils
  ()
where

import Debug.Trace (trace)
import Data.List (elemIndex)

sumChains :: Integer -> [Integer] -> [[Integer]]
sumChains ik = go ik 2
  where
    go :: Integer -> Integer -> [Integer] -> [[Integer]]
    go k 0 lst = case 0 `elemIndex` fmap (k -) lst of 
                    Just n -> [[k]]
                    Nothing  -> [] 
    go k n (a : as) | k - a > 0 = fmap ((:) a) (go (k - a) (n - 1) as) ++ go k n as
                    | otherwise = []
    go k n [] = []