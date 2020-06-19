{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, GADTs #-}

module Generics1RV
  (GFunctor(..), Generics1(..), (:+:)(..), (:*:)(..), fmap', Wrap(..), UnitK(..))
where

import Data.Kind (Type)
import Data.Functor.Identity

class Generics1 (f :: Type -> Type) where
    type Rep f :: Type -> Type

    from :: f x -> Rep f x
    to :: Rep f x -> f x


data UnitK x = Unit
data (f :+: g) x = L (f x) | R (g x)
data (f :*: g) x = P (f x) (g x)
newtype Wrap f a = Wrap (f a)  

fmap' :: (Generics1 f, GFunctor (Rep f)) => (a -> b) -> f a -> f b
fmap' f = to . gfmap f . from

class GFunctor f where
    gfmap :: (a -> b) -> f a -> f b

instance GFunctor UnitK where
    gfmap _ Unit = Unit

instance (GFunctor f, GFunctor g) => GFunctor (f :+: g) where
    gfmap f (L a) = L $ gfmap f a
    gfmap f (R a) = R $ gfmap f a

instance (GFunctor f, GFunctor g) => GFunctor (f :*: g) where
    gfmap f (P fa ga) = P (gfmap f fa) (gfmap f ga)

