module Comonads
  ()
where

import Prelude hiding (take)

class Functor w => Comonad w where
    extract :: w a -> a
    dublicate :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b

    dublicate = extend id 
    extend f = fmap f . dublicate
    
data Store s a = Store (s -> a) s

instance Functor (Store s) where
    fmap g (Store f s) = Store (g . f) s 

instance Comonad (Store s) where
    extract (Store f s) = f s
    dublicate (Store f s) = Store (Store f) s


data Stream a = Cons a (Stream a)

instance Functor Stream where
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
    extract (Cons a _) = a
    dublicate (Cons a as)  = Cons (Cons a as) (dublicate as)

take :: Int -> Stream a -> [a]
take 0 _ = []
take n (Cons a as) = a : take (n - 1) as  

sumN :: Num a => Int -> Stream a -> a
sumN 0 _ = 0 
sumN n (Cons a as) = a + sumN (n - 1) as

averageN :: Fractional a => Int -> Stream a -> a
averageN n st = sumN n st / fromIntegral n

integrals :: Num a => a -> Stream a
integrals init = Cons init (integrals (init + 1))

testStream :: Fractional a => [a]
testStream = take 5 . extend (averageN 2) $ integrals 0