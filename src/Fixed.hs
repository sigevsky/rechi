{-# LANGUAGE DeriveFunctor #-}

module Fixed
  ()
where

fix :: (a -> a) -> a
fix f = f (fix f)

data List a = Nil | Cons a (List a)

sumI :: Num a => List a -> a
sumI Nil = 0
sumI (Cons a as) = a + sumI as

sumL :: Num a => (List a -> a) -> List a -> a
sumL _ Nil = 0
sumL f (Cons a as) = a + f as

sumR :: Num a => List a -> a
sumR = fix sumL

lst = Cons 1 (Cons 2 (Cons 3 Nil))

test = print . sumR $ lst

newtype Fix f = Fx { unfix :: f (Fix f) }

data PlainLst a f = PNil | PCons a f deriving Functor

type PList a = Fix (PlainLst a)

nil :: PList a
nil = Fx PNil

cons :: a -> PList a -> PList a
cons a lst = Fx $ PCons a lst 

foldrF :: (a -> r -> r) -> r -> PList a -> r
foldrF _ ni (Fx PNil) = ni 
foldrF f ni (Fx (PCons a b)) = f a (foldrF f ni b)