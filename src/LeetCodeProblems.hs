{-# LANGUAGE TupleSections #-}

module LeetCodeProblems where
import Control.Lens ( traverseOf, Traversal )
import Data.Monoid (Sum(..))
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.List (intercalate, unfoldr, sortBy, partition, sort, isInfixOf, nub, permutations, minimumBy)
import Debug.Trace (traceShow, traceShowId)
import Data.Char (toLower, toUpper, isDigit)
import Data.Function (on)
import Control.Arrow (first)
import Control.Applicative
import Data.Maybe (listToMaybe)

traceMark :: Show a => String -> a -> a
traceMark str a = traceShow (str <> " " <> show a) a

trapRain :: [Int] -> Int
trapRain = getSum . go . flip zip (repeat 1) 
    where
        go curr = 
          let (v, next) = traverseOf surroundings f (sq curr) 
          in if getSum v == 0 then mempty else v <> go next
        f (End a) = (Sum 0, a)
        f (Inner (a, _) (b, i) (c, _)) | a > b && b < c = let mn = min a c in (Sum $ (mn - b) * i, (mn, i))
        f (Inner _ b _) = (Sum 0, b)
        sq [] = []
        sq xs = foldl' fd [head xs] (tail xs)
        fd (ae@(a, i): es) e@(b, j) = if a == b then (a, i + j):es else e:ae:es

data Window a = End a | Inner a a a

surroundings :: Traversal [a] [b] (Window a) b
surroundings _ [] = pure []
surroundings g l@(x: _) = (:) <$> g (End x) <*> go l
  where
    go [] = pure []
    go [z] =  (:[]) <$> g (End z) 
    go [_, z] =  (:[]) <$> g (End z) 
    go (a: b: c: xs) = (:) <$> g (Inner a b c) <*> go (b: c: xs)

arr :: [Int]
arr = [6,4,2,0,3,2,0,3,1,4,5,3,2,7,5,3,0,1,2,1,3,4,6,8,1,3]

-- | Given a list, find the [Int] that appears an 
--   odd number of times. The tests will always
--   provide such a number, and the list will
--   always contain at least one element.
findOdd :: [Int] -> Int
findOdd xs = head . map fst . filter (odd . snd) . M.toList $ foldl' f M.empty xs 
  where 
    f m v = M.insertWith (\_ x -> x + 1) v 1 m


highAndLow :: String -> String
highAndLow = unwords . map show . f . fmap read . words
  where f :: [Int] -> [Int]
        f xs = let (mn, mx) = foldl g (head xs, head xs) (tail xs) in [mx, mn]
        g (mn, mx) a | mn > a = (a, mx)
        g (mn, mx) a | mx < a = (mn, a)
        g x _ = x 


digpow :: Integer -> Integer -> Integer
digpow n p = if s `mod` n == 0 then s `div` n else -1
  where s = sum . fmap (uncurry (^)) $ zip digits arr
        digits = digs n
        arr = reverse [p..(p + toInteger (length digits) - 1)]


nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = head . map snd . filter ((>= p) . fst) $ zip (unfoldr go p0) [1..]
  where go x = let n = floor $ fromIntegral x * (1 + (0.01 * percent)) + fromIntegral aug in Just (n, n)


accum :: [Char] -> [Char]
accum = intercalate "-" . fmap (\(c:cs) -> toUpper c:cs) . fmap (\(c, i) -> replicate i (toLower c)) . flip zip [1..]


solution :: Integer -> Integer
solution n = sum . filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) $ [1..(n-1)]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x | x < 0 = False
isPrime x = all (\i -> (x `mod` i) /= 0) [2..(ceiling . sqrt . fromIntegral . abs $ x)]


yourOrderPlease :: String -> String
yourOrderPlease = unwords . fmap fst . sortBy (compare `on` snd) . fmap (\w -> (w, nt w)). words
  where nt [] = 0
        nt (e: _) | isDigit e = read [e] :: Int
        nt (e:es) = nt es


sortArray :: [Int] -> [Int]
sortArray xs = snd <$> merge even' (zip oddIdx sortedOdd)
  where (odd', even') = partition (odd . snd) $ zip [0..] xs 
        sortedOdd = sort $ fmap snd odd'
        oddIdx = fst <$> odd'

merge :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
merge xs     []     = xs
merge []     ys     = ys
merge (xe@(xi, _):xs) yl@((yi, _):_) | xi <= yi = xe : merge xs yl
merge xs ys = merge ys xs


orderWeight :: [Char] -> [Char]
orderWeight = unwords . fmap snd . sort . fmap (first (sum . strToDigits) . dup) . words
  where 
    strToDigits :: String -> [Int]
    strToDigits s = (\x -> read [x]) <$> s
    dup a = (a, a)


narcissistic :: Integral n => n -> Bool
narcissistic n = n == sum (fmap (^ length digts) digts)
  where digts = digs n


-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = nub . filter (\e -> any (e `isInfixOf`) a2) $ sort a1

naiveNextBigInt :: Int -> Int
naiveNextBigInt n = fst . headOrDef (n, 0) . sortBy (compare `on` snd) . filter ((>0) . snd) $ (\x -> (x, x - n)) . undigits <$> permutations (digs n)
  where headOrDef a [] = a
        headOrDef _ (x:_) = x

-- 3019
-- 3109
-- 3091

nextBigInt :: Int -> Int
nextBigInt n = undigits . f $ digs n
  where dgs = zip (digs n) [0..]
        dists [] = []
        dists (x:xs) = fmap (distance10 x) xs ++ dists xs
        mbHead = listToMaybe $ sortBy (compare `on` \(a, _, _) -> a) $ filter (\(a, _, _) -> a > 0) (dists dgs)
        f = case mbHead of
              Just (_, i, j) -> thr i j
              Nothing -> const [-1]

thr :: Int -> Int -> [Int] -> [Int]
thr i j xs = foldl f [] (zip xs [0..])
  where 
    pE = xs !! j
    f ls (_, k) | k == j = ls
    f ls (a, k) | k == i =  pE: sort (a:ls)
    f ls (a, k) | k < i || k > i = a:ls

distance10 :: (Int, Int) -> (Int, Int) -> (Int, Int, Int)
distance10 (a, j) (b, i) = ((10^i - 10^j) * (a - b), i, j)

digs :: Integral b => b -> [b]
digs = map (`mod`10) . takeWhile (>0) . iterate (`div`10)

undigits :: Num a => [a] -> a
undigits = foldl (\acc n -> acc * 10 + n) 0


nextBigger :: Int -> Int
nextBigger = maybe (-1) read . n . show where
  n s = do
    (d : ds) <- return s
    (d :) <$> n ds <|> do
      let (a, b) = span (> d) ds
      d2 : a2 <- return (reverse a)
      return (d2 : sort (d : a2 ++ b))