{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module TwTExercies 
    ()
where

import Data.Kind ( Constraint , Type )
import Data.Maybe ( listToMaybe )

-- Exercise 3-i
newtype T1 a = T1 (Int -> a)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T1 where
    fmap f (T1 g) = T1 (f . g)

-- here value of a is produced to the callback function thus its covariant
instance Functor T5 where
    fmap f (T5 g) = T5 $ \bi -> g (bi . f)
 
-- Exercise 2.4-i
type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

-- Exercise 7.1-i 

data Any where
    Any :: a -> Any

elimAny :: Any -> (forall a . a -> r) -> r
elimAny (Any a) f = f a


data HasShow where
    HasShow :: Show a => a -> HasShow

showCont :: HasShow -> String
showCont (HasShow a) = show a

data Has (xs :: Type -> Constraint) where
    Has :: (xs a) => a -> Has xs


hasShow :: [Has Show]
hasShow = [Has "Str", Has 1]

elimHas :: Has c -> (forall t . c t => t -> r) -> r
elimHas (Has a) f = f a 

-- Defunc

-- Exercise 10.1-i defunc [a] -> Maybe a

newtype ListToMaybe a = LTM [a]

class Eval a l | a -> l where
    eval :: a -> l

instance Eval (ListToMaybe a) (Maybe a) where
    eval (LTM a) = listToMaybe a


testDefunc = eval (LTM [1, 2, 3])