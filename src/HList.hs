{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE PolyKinds #-}


module HList
    ()
where

import Data.Typeable

import Data.Kind ( Constraint , Type )
import Data.Proxy (Proxy(..))

infixr 5 :#

data HList (a :: [Type]) where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)

class Premier xs a where
    prem :: xs -> Maybe a

instance Premier (HList '[]) a where
    prem _ = Nothing

instance {-# OVERLAPPING #-} Premier (HList (x ': xs)) x where
    prem (x :# _) = Just x

instance Premier (HList xs) a => Premier (HList (x ': xs)) a where
    prem (_ :# xs) = prem xs


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
