{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}

module Layers.Layers where

import Layers.HList ( gprem, HList(..), Premier, UnionTys, Proj (hProj), HUnion (hUnion), HPermute (hPermute), hHead, hTail )
import Data.Functor.Identity ( Identity(Identity) )
import Control.Monad ((>=>))
import Control.Applicative (liftA2)

type family FnResult a :: * where
  FnResult (a -> b) = FnResult b
  FnResult x = x

class Unfold xs f where
  unfold :: HList xs -> f -> HList (FnResult f ': xs)

instance {-# OVERLAPPING #-} (Premier xs a, Unfold xs b) => 
  Unfold xs (a -> b) where
    unfold xs f = unfold xs b
      where b = f $ gprem xs

instance (b ~ FnResult b) => Unfold xs b where
  unfold hlst b = b :# hlst

-- manual
fromCtx :: HList xs -> ((?ctx :: HList xs) => a) -> HList (a ': xs)
fromCtx xs f = let ?ctx = xs in f :# xs

to :: (Premier xs a, ?ctx :: HList xs) => (a -> b) -> b
to f = f $ gprem ?ctx

-- Simple layer: HList xs -> HList ys
type family FnPfx (a :: *) :: [*] where
  FnPfx (a -> b) = a ': FnPfx b
  FnPfx a = '[]

class HSplit f where
  hSplit :: f -> HList (FnPfx f) -> HList (FnResult f ': FnPfx f)

instance (b ~ FnResult b) => HSplit b where
  hSplit b ctx = b :# ctx

instance {-# OVERLAPPING #-} HSplit b => HSplit (a -> b) where
  hSplit f ctx@(x :# xs) = hHead e :# x :# hTail e
    where b = f $ gprem ctx
          e = hSplit b xs

subdvd :: HSplit f => f -> HList (FnPfx f) -> HList '[FnResult f]
subdvd f hs = hHead e :# HNil
  where e = hSplit f hs

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

-- compose
(.#) :: HPermute bs xs => (HList xs -> HList ys) -> (HList as -> HList bs) -> HList as -> HList ys
(.#) f g = f . hPermute . g

-- Effectful Layers
newtype Layer m i out = Layer { runLayer :: HList i -> m (HList out) }

type family FnResultM (m :: * -> *) (a :: *) where
  FnResultM m (a -> b) = FnResultM m b
  FnResultM m (m a) = a

class HSplitM m f where
  hSplitM :: f -> HList (FnPfx f) -> m (HList (FnResultM m f ': FnPfx f))

instance (Functor m, FnResultM m (m a) ~ a) => HSplitM m (m a) where
  hSplitM ma ctx = (:# ctx) <$> ma

instance {-# OVERLAPPING #-} (Applicative m, HSplitM m b) => HSplitM m (a -> b) where
  hSplitM f ctx@(x :# xs) = (\i' x' rest' -> i' :# x' :# rest') <$> (hHead <$> e) <*> pure x <*> (hTail <$> e)
    where b = f $ gprem ctx
          e = hSplitM b xs

subdvdM :: (HSplitM m f, Functor m) => f -> HList (FnPfx f) -> m (HList '[FnResultM m f])
subdvdM f hs = (:# HNil) <$> e  
  where e = hHead <$> hSplitM f hs

fromValue :: a -> Layer Identity '[] '[a]
fromValue a = Layer $ \_ -> Identity $ a :# HNil

fromValueM :: Functor m => m a -> Layer m '[] '[a]
fromValueM a = Layer $ \_ -> (:# HNil) <$> a

fromFn :: HSplit f => f -> Layer Identity (FnPfx f) '[FnResult f]
fromFn f = Layer $ \ls -> Identity (subdvd f ls)

fromFnM :: (HSplitM m f, Functor m) => f -> Layer m (FnPfx f) '[FnResultM m f ]
fromFnM f = Layer $ subdvdM f

class Embed m t where
  embed :: m a -> t a

instance {-# OVERLAPS #-} (m ~ t) => Embed m t where
  embed = id

instance {-# OVERLAPPING #-} Applicative t => Embed Identity t where
  embed (Identity a) = pure a

-- compose vertically
(.$) :: (Monad m2, HPermute t1 t2, Embed m1 m2) => Layer m2 t2 o -> Layer m1 i t1 -> Layer m2 i o
(Layer a) .$ (Layer b) = Layer $ embed . b >=> (a . hPermute)

-- compose horizontally
($>) ::
 (Proj (UnionTys xs as) xs, Proj (UnionTys xs as) as, HUnion ys bs, Applicative m) => 
  Layer m xs ys -> 
  Layer m as bs -> 
  Layer m (UnionTys xs as) (UnionTys ys bs)
($>) (Layer fx) (Layer ga) = Layer $ \ctx -> 
    let
       ys = fx $ hProj ctx
       bs = ga $ hProj ctx 
    in liftA2 hUnion ys bs

provideLayer :: (HPermute xs ys) => Layer m ys as -> HList xs -> m (HList as)
provideLayer (Layer f) = f . hPermute

sealLayer :: Layer m '[] o -> m (HList o)
sealLayer (Layer a) = a HNil