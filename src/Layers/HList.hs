{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE PolyKinds #-}


module Layers.HList where

import Data.Typeable

import Data.Kind ( Constraint , Type )
import Data.Proxy (Proxy(..))

infixr 5 :#

data HList (a :: [Type]) where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)

hTail :: HList (a ': as) -> HList as
hTail (_ :# xs) = xs

hHead :: HList (a ': as) -> a
hHead (x :# _) = x

class Premier xs a where
    prem :: HList xs -> a

gprem :: forall a xs . Premier xs a => HList xs -> a
gprem = prem

instance {-# OVERLAPPING #-} Premier (x ': xs) x where
    prem (x :# _) = x

instance Premier xs a => Premier (x ': xs) a where
    prem (_ :# xs) = prem xs

type family HasTy (l :: [*]) b :: Bool where
  HasTy '[] _ = False
  HasTy (a ': as) a = True
  HasTy (a ': as) b = HasTy as b

type family AppendTy (l :: [*]) b :: [*] where
    AppendTy '[] b = b ': '[]
    AppendTy (a ': as) a = a ': as
    AppendTy (a ': as) b = a ': AppendTy as b

type family UnionTys (l :: [*]) (t :: [*]) :: [*] where
    UnionTys acc '[] = acc
    UnionTys acc (b ': bs) = UnionTys (AppendTy acc b) bs 

class ApNonRep xs a where
    apNp :: HList xs -> a -> HList (AppendTy xs a)

instance ApNonRep '[] a where
    apNp ls a = a :# ls

instance {-# OVERLAPPING #-} ApNonRep (x ': xs) x where
    apNp ls _ = ls

instance (AppendTy (x ': xs) b ~ (x ': AppendTy xs b), ApNonRep xs b) =>
 ApNonRep (x ': xs) b where
    apNp (a :# as) b = a :# apNp as b

class HUnion as bs where
    hUnion :: HList as -> HList bs -> HList (UnionTys as bs)

instance UnionTys xs '[] ~ xs => HUnion xs '[] where
    hUnion xs _ = xs

instance (HUnion (AppendTy xs a) as, UnionTys xs (a ': as) ~ UnionTys (AppendTy xs a) as, ApNonRep xs a) => 
    HUnion xs (a ': as)  where
        hUnion acc (a :# as) = hUnion (apNp acc a) as

instance Show (HList '[]) where
    show _ = "Nil"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
    show (x :# xs) = show x <> ", " <> show xs

-- TODO: write function for permutation without using typeclasses (hint: AllOf Premier xs [..] with tail equivalence check)
class HPermute xs ys where
    hPermute :: HList xs -> HList ys

instance HPermute as '[] where
    hPermute a = HNil

instance (Premier xs a, HPermute xs as) =>  HPermute xs (a ': as) where
    hPermute xs = gprem xs :# hPermute xs

class Proj xss xs where
    hProj :: HList xss -> HList xs

instance Proj a '[] where
    hProj _ = HNil

instance {-# OVERLAPPING #-} Proj xs bs => Proj (a ': xs) (a ': bs) where
    hProj (a :# as) = a :# hProj as

instance Proj xs (b ': bs) => Proj (a ': xs) (b ': bs) where
    hProj (a :# as) = hProj as  

type family All (c :: Type -> Constraint ) (ts :: [ Type ]) :: Constraint where
    All cs '[] = ()
    All cs (a ': as) = (cs a, All cs as)

instance (All Eq xs) => Eq (HList xs) where
    HNil == HNil = True
    (x :# xs) == (y :# ys) = (x == y) && (xs == ys)


instance (All Ord xs, All Eq xs) => Ord (HList xs) where
    HNil <= HNil = True
    (x :# xs) <= (y :# ys) =  if x == y then xs <= ys else x <= y 