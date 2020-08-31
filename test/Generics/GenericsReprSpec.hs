{-# LANGUAGE DataKinds #-}

module Generics.GenericsReprSpec
  (spec)
where

import Test.Hspec
import Generics.GenericsRep
import Prelude hiding (seq)

data Term = Var String | App Term Term | Abs String Term deriving Show

instance GenericA Term where
  type Code Term = Sum (Atom String) (Sum (Prod (Atom Term) (Atom Term)) (Prod (Atom String) (Atom Term)))

  from (Var a) = Left a
  from (App a b) = Right . Left $ (a, b)
  from (Abs str b) = Right . Right $ (str, b)

  to (Left a) = Var a
  to (Right (Left (a, b))) = App a b
  to (Right (Right (str, b))) = Abs str b

instance Eq Term where a == b = a `eq'` b

-- Test data

data Foo a = Bar a a a | Baz a a | Aux

instance GenericA (Foo a) where
  type Code (Foo a) = Sum (Prod (Atom a) (Prod (Atom a) (Atom a))) (Sum (Prod (Atom a) (Atom a)) Unit)

  from (Bar a b c) = Left (a, (b, c))
  from (Baz a b) = Right . Left $ (a, b)
  from Aux = Right . Right $ ()

  to (Left (a, (b, c))) = Bar a b c
  to (Right (Left (a, b))) = Baz a b
  to (Right (Right ())) = Aux

data Colour = RED | BLUE | GREEN deriving (Show, Eq)

instance GenericA Colour where
  type Code Colour = Sum (Cons "Red" Unit) (Sum (Cons "Green" Unit) (Cons "Blue" Unit))

  from RED = Left ()
  from GREEN = Right . Left $ ()
  from BLUE = Right . Right $ ()

  to (Left ()) = RED
  to (Right (Left ())) = GREEN
  to (Right (Right ())) = BLUE

spec :: Spec
spec =
    describe "Representation based generics" $ do
        it "should run equality check towards generic representations" $ example $
            (Abs "Second" (App (Var "x") (Var "y")) `eq'` Abs "Second" (Var "z")) `shouldBe` False
        it "should run equality check with singleton based equality implementation" $ example $
            (Abs "Second" (App (Var "x") (Var "y")) `seq` Abs "Second" (Var "z")) `shouldBe` False
        it "should sum up all int values non-recursively" $ example $
          total (Bar 2 3 5 :: Foo Int) `shouldBe` 10
        it "should provide all 3 colours" $ example $
          enum `shouldContain` [RED, GREEN, BLUE]

