{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, UndecidableInstances #-}

module Generics1Spec
  (spec)
where

import Data.Functor.Identity

import Test.Hspec
import Generics1RV

type Wridn = Wrap Identity

instance Generics1 Maybe where
    type Rep Maybe = UnitK :+: Wridn

    from (Just a) = R (Wrap . Identity $ a)
    from _        = L Unit

    to (R (Wrap (Identity a))) = Just a
    to (L Unit) = Nothing  



data List a = Nil | a :|: (List a) deriving (Eq, Show)
infixr 7 :|:

instance Generics1 List where
    type Rep List = UnitK :+: (Wrap Identity :*: Wrap List) 
    
    from Nil = L Unit
    from (x :|: xs) = R (P (Wrap $ Identity x) (Wrap xs))

    to (L Unit) = Nil
    to (R (P (Wrap (Identity x)) (Wrap xs))) = x :|: xs 

instance Functor f => GFunctor (Wrap f) where
    gfmap f (Wrap xs) = Wrap $ fmap f xs

instance Functor List where
    fmap = fmap'

spec :: Spec
spec =
    describe "Hight order derivation" $ do 
        it "should map list to its reverse under the option" $ example $
            fmap' reverse (Just [1, 2, 3]) `shouldBe` Just [3, 2, 1]
        it "should not map under nothing" $ example $
            fmap' reverse Nothing `shouldBe` (Nothing :: Maybe [Integer])
        it "should add one to each element" $ example $
            fmap' (+1) (1 :|: 2 :|: 3 :|: Nil) `shouldBe` (2 :|: 3 :|: 4 :|: Nil :: List Integer)
        it "should show each element" $ example $
            fmap' show (1 :|: 2 :|: 3 :|: Nil) `shouldBe` ("1" :|: "2" :|: "3" :|: Nil :: List String)