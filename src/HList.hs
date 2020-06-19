{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE PolyKinds #-}


module HList
    ()
where

import Data.Kind ( Constraint , Type )

data HList (a :: [Type]) where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)

infixr 5 :#

headH :: HList (a ': as) -> a
headH (a :# _) = a


instance Show (HList '[]) where
    show _ = "Nil"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
    show (x :# xs) = show x <> ", " <> show xs


type family All (c :: Type -> Constraint ) (ts :: [ Type ]) :: Constraint where
    All cs '[] = ()
    All cs (a ': as) = (cs a, All cs as)

instance (All Eq xs) => Eq (HList xs) where
    HNil == HNil = True
    (x :# xs) == (y :# ys) = (x == y) && (xs == ys)


instance (All Ord xs, All Eq xs) => Ord (HList xs) where
    HNil <= HNil = True
    (x :# xs) <= (y :# ys) =  if x == y then xs <= ys else x <= y 

