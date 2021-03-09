{-# LANGUAGE DataKinds #-}

module Layers.LayersSpec (spec) where

import Test.Hspec
import Layers.Layers
import Layers.HList
import Data.Functor.Identity

data A = A deriving (Show, Eq)
data B = B deriving (Show, Eq)
data C = C deriving (Show, Eq)

newtype ReqA = ReqA A deriving (Show, Eq)
data ReqAB = ReqAB A B deriving (Show, Eq)


ctx :: HList '[A, B, C]
ctx = A :# B :# C :# HNil

test1 :: HList '[ReqAB, A, B, C]
test1 = unfold ctx ReqAB

test2 :: HList '[ReqAB, A, B, C]
test2 = fromCtx ctx (to . to $ ReqAB)

test3 :: HList '[A, B] -> HList '[ReqAB]
test3 = subdvd ReqAB

test4 :: HList '[A, B] -> HList '[ReqAB, A, B]
test4 = hSplit ReqAB

test5 :: HList '[ReqAB, ReqA]
test5 = ajoin a b `provide` ctx
  where
    a :: HList [A, B] -> HList '[ReqAB]
    a = subdvd ReqAB

    b :: HList '[A] -> HList '[ReqA]
    b = subdvd ReqA

    ctx = B :# A :# HNil

test6 :: Layer IO '[] '[ReqAB]
test6 = fromFnM (\a b -> (pure () :: IO ()) >> return (ReqAB a b)) .$ (fromValue A $> fromValue B)



e :: Layer IO '[] '[A]
e = fromValueM (putStrLn "hell" >> return A) $> fromValueM (putStrLn "hell" >> return A)

blah :: Layer IO '[] '[ReqAB]
blah = fromFn ReqAB .$ e

spec :: Spec
spec = describe "Check helper hlist operations yield valid types" $ do
         it "should construct layer from effect" $ example $ 
          (hHead <$> sealLayer test6) `shouldReturn` ReqAB A B