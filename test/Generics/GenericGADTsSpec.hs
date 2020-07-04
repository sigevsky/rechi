{-# LANGUAGE DataKinds, MultiParamTypeClasses, UndecidableInstances #-}
module Generics.GenericGADTsSpec
  (spec)
where

import Generics.GenericGADTs
import Test.Hspec
import Data.Kind (Type, Constraint)

data Term = Var String | App Term Term | Abs String Term deriving Show

instance (c String, c Term) => Generics c Term where
  type Code Term = Sum (Atom String) (Sum (Prod (Atom Term) (Atom Term)) (Prod (Atom String) (Atom Term)))

  from (Var s) = LGSum . GAtom $ s
  from (App t1 t2) = RGSum . LGSum $ GProd (GAtom t1) (GAtom t2)
  from (Abs s t) = RGSum . RGSum $ GProd (GAtom s) (GAtom t)

  to (LGSum (GAtom s)) = Var s
  to (RGSum (LGSum ((GProd (GAtom t1) (GAtom t2))))) = App t1 t2
  to (RGSum (RGSum (GProd (GAtom s) (GAtom t2)))) = Abs s t2

instance Eq Term where a == b = a `geq` b

spec :: Spec
spec = describe "GADTs based generic representations" $
         it "should run equality check" $ example $
           (Abs "Second" (App (Var "x") (Var "y")) `geq` Abs "Second" (Var "z")) `shouldBe` False


