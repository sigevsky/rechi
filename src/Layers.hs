{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Layers
  ()
where

import HList ( gprem, HList(..), Premier, UnionTys, Proj (hProj), HUnion (hUnion), HPermute (hPermute) )
import Control.Monad.Reader
import Control.Monad.Except

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show

newtype ReqA = ReqA A deriving Show
data ReqAB = ReqAB A B deriving Show

type family LastF a :: * where
  LastF (a -> b) = LastF b
  LastF x = x

class Unfold xs f where
  unfold :: HList xs -> f -> HList (LastF f ': xs)

instance {-# OVERLAPPING #-} (Premier xs a, Unfold xs b) => 
  Unfold xs (a -> b) where
    unfold xs f = unfold xs b
      where b = f $ gprem xs

instance (b ~ LastF b) => Unfold xs b where
  unfold hlst b = b :# hlst

ctx :: HList '[A, B, C]
ctx = A :# B :# C :# HNil

test1 :: HList '[ReqAB, A, B, C]
test1 = unfold ctx ReqAB

-- manual
fromCtx :: HList xs -> ((?ctx :: HList xs) => a) -> HList (a ': xs)
fromCtx xs f = let ?ctx = xs in f :# xs

to :: (Premier xs a, ?ctx :: HList xs) => (a -> b) -> b
to f = f $ gprem ?ctx

test2 :: HList '[ReqAB, A, B, C]
test2 = fromCtx ctx (to . to $ ReqAB)

-- Layer: HList xs -> HList ys
type family PfxF (a :: *) :: [*] where
  PfxF (a -> b) = a ': PfxF b
  PfxF a = '[]

class FSplit f where
  tdvd :: f -> HList (PfxF f) -> HList (LastF f ': PfxF f)

instance (b ~ LastF b) => FSplit b where
  tdvd b ctx = b :# ctx

instance {-# OVERLAPPING #-} FSplit b => FSplit (a -> b) where
  tdvd f ctx@(x :# xs) = let
                           e = hsplit $ tdvd b xs
                           i = fst e
                           rest = snd e
                         in i :# x :# rest
    where b = f $ gprem ctx

subdvd :: FSplit f => f -> HList (PfxF f) -> HList (LastF f ': '[])
subdvd f hs = fst e :# HNil
  where e = hsplit $ tdvd f hs


hsplit :: HList (a ': as) -> (a, HList as)
hsplit (x :# xs) = (x, xs)

test3 :: HList '[A, B] -> HList '[ReqAB]
test3 = subdvd ReqAB

test4 :: HList '[A, B] -> HList '[ReqAB, A, B]
test4 = tdvd ReqAB

-- hlist helper foo
ajoin :: forall xs ys as bs .
  (Proj (UnionTys xs as) xs, Proj (UnionTys xs as) as, HUnion ys bs) => 
  (HList xs -> HList ys) -> 
  (HList as -> HList bs) -> 
  HList (UnionTys xs as) -> 
  HList (UnionTys ys bs)
ajoin fx ga ctx = hUnion ys bs
  where
    ys = fx $ hProj ctx
    bs = ga $ hProj ctx

provide :: (HPermute xs ys) => (HList ys -> HList as) -> HList xs -> HList as
provide f = f . hPermute 

test5 :: HList '[ReqAB, ReqA]
test5 = ajoin a b `provide` ctx
  where
    a :: HList [A, B] -> HList '[ReqAB]
    a = subdvd ReqAB

    b :: HList '[A] -> HList '[ReqA]
    b = subdvd ReqA

    ctx = B :# A :# HNil

-- compose
(.#) :: HPermute bs xs => (HList xs -> HList ys) -> (HList as -> HList bs) -> HList as -> HList ys
(.#) f g = f . hPermute . g

-- -- newtype Layer m e i out = Layer { runLayer :: HList i -> m (Either e out)}

-- newtype LayerLike m e i out = Layer (ReaderT i (ExceptT e m) out) 
--   deriving (Functor, Applicative, Monad, MonadError e, MonadReader i)

-- -- type Layer m e i out = LayerLike m e (HList i) (HList out)

