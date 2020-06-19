{-# LANGUAGE UndecidableInstances, DataKinds, TypeApplications #-}

module GenericsRV
  (GenericS(..), Wrap(..), Cons(..), GEq, GCmp, GEnum, GBaseCases, GMemp, GTotal,
   eq, cmp, enum, memp, total, baseCases, parseEnum)
where

import Data.List (find)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

class GenericS a where
    type Rep a :: *

    from :: a -> Rep a
    to :: Rep a -> a

newtype Wrap a = Wrap a deriving Show

eq :: (GenericS a, GEq (Rep a)) => a -> a -> Bool
eq a b = geq (from a) (from b)


class GEq a where
    geq :: a -> a -> Bool

instance (GEq a, GEq b) => GEq (Either a b) where
    geq (Left a)  (Left b)  = geq a b
    geq (Right a) (Right b) = geq a b
    geq _         _         = False 

instance (GEq a, GEq b) => GEq (a, b) where
    geq (a, b) (x, y) = geq a x && geq b y

-- collapses with all above definitions
-- instance Eq a => GEq a where
--     geq = eq

-- Its is called indirectly via typeclass Eq so as to let
-- recursion finish on a ~ regular type thereby forming a base case
instance Eq a => GEq (Wrap a) where
    geq (Wrap a) (Wrap b) = a == b


cmp :: (GenericS a, GCmp (Rep a)) => a -> a -> Ordering
cmp a b = gcmp (from a) (from b)

class GCmp a where
    gcmp :: a -> a -> Ordering

instance (GCmp a, GCmp b) => GCmp (Either a b) where
    gcmp (Left a)  (Left b)  = gcmp a b
    gcmp (Right a) (Right b) = gcmp a b
    gcmp (Left _)  (Right _) = GT 
    gcmp (Right _) (Left _)  = LT

instance (GCmp a, GCmp b) => GCmp (a, b) where
    gcmp (a, b) (x, y) = case gcmp a x of
                            EQ -> gcmp b y
                            cr  -> cr

instance Ord a => GCmp (Wrap a) where
    gcmp (Wrap a) (Wrap b) = a `compare` b

instance GEq () where
    geq () () = True

instance GCmp () where
    gcmp () () = EQ

enum :: (GenericS a, GEnum (Rep a)) => [a]
enum = to <$> genum

class GEnum a where
    genum :: [a]

instance GEnum () where
    genum = [()]

-- This instance does not take into account that enum could be represented differently
-- e.g Either () (EIther () ()) ~ Either (Either () ()) ()
-- to account for that we need (Left <$> gbaseCases) ++ (Right <$> gbaseCases)
instance (GEnum a, GEnum b) => GEnum (Either a b) where
    genum = (Left <$> genum) ++ (Right <$> genum)


-- Exercise 3: Same as enum but works for all types returning only no args constructors
baseCases :: (GenericS a, GBaseCases (Rep a)) => [a]
baseCases = to <$> gbaseCases

class GBaseCases a where
    gbaseCases :: [a]

instance GBaseCases () where
    gbaseCases = [()]

instance GBaseCases (a, b) where
    gbaseCases = []

instance GBaseCases (Wrap a) where
    gbaseCases = []

instance (GBaseCases a, GBaseCases b) => GBaseCases (Either a b) where
    gbaseCases = (Left <$> gbaseCases) ++ (Right <$> gbaseCases)

-- Exercise 4
memp :: (GenericS a, GMemp (Rep a)) => a
memp = to gmemp

class GMemp a where
    gmemp :: a

instance GMemp () where
    gmemp = ()

instance (GMemp a, GMemp b) => GMemp (a, b) where
    gmemp = (gmemp, gmemp)

instance (Monoid a) => GMemp (Wrap a) where
    gmemp = Wrap mempty

-- Exercise 5
total :: (GenericS a, GTotal (Rep a)) => a -> Int
total = gtotal . from

class GTotal a where
    gtotal :: a -> Int

instance (GTotal a, GTotal b) => GTotal (Either a b) where
    gtotal = either gtotal gtotal

instance (GTotal a, GTotal b) => GTotal (a, b) where
    gtotal (a, b) = gtotal a + gtotal b

instance GTotal (Wrap Int) where
    gtotal (Wrap i) = i

instance GTotal () where
    gtotal () = 0

newtype Cons (d :: Symbol) a = Cons a

parseEnum :: forall a . (GenericS a, GEnum (Rep a), GHasName (Rep a)) => String -> Maybe a
parseEnum s = fmap to . find (gparse s) . fmap from $ (enum :: [a])

class GHasName a where
    gparse :: String -> a -> Bool

instance (GHasName a, GHasName b) => GHasName (Either a b) where
    gparse s = either (gparse s) (gparse s) 

instance (KnownSymbol n, GHasName a) => GHasName (Cons n a) where
    gparse s (Cons a) = symbolVal (Proxy @n) == s || gparse s a

instance GHasName () where
    gparse _ () = False

instance GCmp a => GCmp (Cons n a) where
    (Cons a) `gcmp ` (Cons b) = a `gcmp` b

instance GEnum a => GEnum (Cons n a) where
    genum = Cons <$> genum 

instance GEq a => GEq (Cons n a) where
    (Cons a) `geq ` (Cons b) = a `geq` b