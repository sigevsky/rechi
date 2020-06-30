{-# LANGUAGE UndecidableInstances, DataKinds #-}

module GenericsSpec
  (spec)
where

import Test.Hspec
import Generics.GenericsRV

data Term = Var String | App Term Term | Abs String Term deriving Show

instance GenericS Term where
    type Rep Term = Either (Wrap String) (Either (Wrap Term, Wrap Term) (Wrap String, Wrap Term))

    from (Var str) = Left . Wrap $ str
    from (App a b) = Right . Left $ (Wrap a, Wrap b)
    from (Abs str a) = Right . Right $ (Wrap str, Wrap a)

    to (Left (Wrap str)) = Var str
    to (Right (Left (Wrap a, Wrap b))) = App a b
    to (Right (Right (Wrap str, Wrap a))) = Abs str a

instance Eq Term where
    (==) = eq

instance (GCmp (Rep Term)) => Ord Term where
    a `compare` b = a `cmp` b

data Colour = RED | BLUE | GREEN deriving (Show, Eq)

instance GenericS Colour where
    type Rep Colour = Either (Cons "RED" ()) (Either (Cons "BLUE" ()) (Cons "GREEN" ()))

    from RED = Left . Cons $ ()
    from BLUE = Right . Left . Cons $ ()
    from GREEN = Right . Right . Cons $ ()

    to (Left (Cons ())) = RED
    to (Right (Left (Cons ()))) = BLUE
    to (Right (Right (Cons ()))) = GREEN

data TestBaseCases = A | S TestBaseCases String | B deriving (Show, Eq)

instance GenericS TestBaseCases where
    type Rep TestBaseCases = Either () (Either (Wrap TestBaseCases, Wrap String) ())

    from A = Left ()
    from (S rc str) = Right . Left $ (Wrap rc, Wrap str)
    from B = Right . Right $ ()

    to (Left ()) = A
    to (Right (Left (Wrap rc, Wrap str))) = S rc str
    to (Right (Right ())) = B


data Foo a = Bar a a a | Baz a a | Aux

instance GenericS (Foo a) where
    type Rep (Foo a) = Either (Wrap a, (Wrap a, Wrap a)) (Either (Wrap a, Wrap a) ())

    from (Bar a b c) = Left (Wrap a, (Wrap b, Wrap c))
    from (Baz a b) = Right . Left $ (Wrap a, Wrap b)
    from Aux = Right . Right $ ()

    to (Left (Wrap a, (Wrap b, Wrap c))) = Bar a b c
    to (Right (Left (Wrap a, Wrap b))) = Baz a b
    to (Right (Right ())) = Aux


spec :: Spec
spec = do
    describe "Equality checks" $ do
        it "should provide lambda equality check" $ example $
            (Abs "Second" (App (Var "x") (Var "y")) `eq` Abs "Second" (Var "z")) `shouldBe` False
        it "should provide lambda equality check" $ example $
            (RED `eq` BLUE) `shouldBe` False

    describe "Equality checks" $ do
        it "should compare two different colours" $ example $
            (RED `cmp` BLUE) `shouldBe` GT
        it "should compare two similar colours" $ example $
            (RED `cmp` RED) `shouldBe` EQ
        it "should compare two lambdas" $ example $
            (Abs "Second" (App (Var "x") (Var "y")) `cmp` Abs "Second" (Var "z")) `shouldBe` LT


    describe "Enum extraction" $
        it "should provide all 3 colours" $ example $
          enum `shouldContain` [RED, BLUE, GREEN]

    describe "Base case extraction" $
        it "should extract constructors with no arguments only" $ example $
          baseCases `shouldContain` [A, B]

    describe "Count total" $
        it "should sum up all int values non-recursively" $ example $
          total (Bar 2 3 5 :: Foo Int) `shouldBe` 10

    describe "String parse" $
        it "should parse string to an enum" $ example $ do
          parseEnum "RED" `shouldBe` Just RED
          parseEnum "GREEN" `shouldBe` Just GREEN
          parseEnum "BLUE" `shouldBe` Just BLUE
          parseEnum @Colour "blah" `shouldBe` Nothing


