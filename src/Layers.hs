{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layers
  ()
where

import HList ( gprem, HList(..), Premier )

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show

newtype ReqA = ReqA A deriving Show
data ReqAB = ReqAB A B deriving Show

type family LastF a :: * where
  LastF (a -> b) = LastF b
  LastF x = x

class Unfold xs f where
  unfold :: HList xs -> f -> LastF f

instance {-# OVERLAPPING #-} (Premier xs a, Unfold xs b) => 
  Unfold xs (a -> b) where
    unfold xs f = unfold xs b
      where b = f $ gprem xs

instance (b ~ LastF b) => Unfold xs b where
  unfold hlst b = b

ctx :: HList '[A, B, C]
ctx = A :# B :# C :# HNil

blah :: HList '[ReqAB, A, B, C]
blah = unfold ctx ReqAB :# ctx
