{-# LANGUAGE GADTs, DataKinds, MultiParamTypeClasses, UndecidableInstances #-}
module Generics.GenericGADTs
  (Repr(..), GRepr(..), Generics(..), geq)
where

import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))

data Repr =
    Sum Repr Repr |
    Prod Repr Repr |
    Cons Symbol Repr |
    Unit |
    Atom Type

data GRepr (c :: Type -> Constraint) (r :: Repr) where
  GUnit :: GRepr c Unit
  GAtom :: c a => a -> GRepr c (Atom a)
  GCons :: Proxy n -> GRepr c r -> GRepr c (Cons n r)
  GProd :: GRepr c a -> GRepr c b -> GRepr c (Prod a b)
  LGSum  :: GRepr c l -> GRepr c (Sum l r)
  RGSum  :: GRepr c r -> GRepr c (Sum l r)


class Generics (c :: Type -> Constraint) a where
 type Code a :: Repr

 from :: a -> GRepr c (Code a)
 to   :: GRepr c (Code a) -> a

-- its a bit better then plain Either/Product in the sense that we brought (,) and Either constructors to same type thus adding
-- an option of implementing functions on generic representation through plain pattern matching instead of typeclasses
geq' :: GRepr Eq a -> GRepr Eq a -> Bool
geq' (LGSum a) (LGSum b) = a `geq'` b
geq' (RGSum a) (RGSum b) = a `geq'` b
geq' (LGSum _) (RGSum _) = False
geq' (RGSum _) (LGSum _) = False
geq' (GProd a b) (GProd x y) = a `geq'` x && b `geq'` y
geq' (GCons _ a) (GCons _ b) = a `geq'` b
geq' (GAtom a) (GAtom b) = a == b
geq' GUnit GUnit = True

geq :: (Generics Eq a) => a -> a -> Bool
geq a b = geq' (from a) (from b)

class GEnum a where
  genum :: [a]

-- almost the same thing as writing enum for Either/Product based representation
-- with the difference that here instances for which we could create enum are explicitly restricted by the choice of Repr
-- i.e we cant create instance for GEnum (GRepr c blah)
instance (GEnum (GRepr c a), GEnum (GRepr c b)) => GEnum (GRepr c (Sum a b)) where
  genum = (LGSum <$> genum) ++ (RGSum <$> genum)

instance GEnum (GRepr c Unit) where
  genum = [GUnit]

