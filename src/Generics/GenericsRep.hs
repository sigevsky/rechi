{-# LANGUAGE DataKinds, PolyKinds, GADTs, AllowAmbiguousTypes, MultiParamTypeClasses, UndecidableInstances #-}

module Generics.GenericsRep
  (Repr(..), Interpret, GenericA(..), Rep, eq', GEq, seq, total, enum)
where

import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Prelude hiding (seq)
import Debug.Trace (trace)

data Repr =
    Sum Repr Repr |
    Prod Repr Repr |
    Cons Symbol Repr |
    Unit |
    Atom Type

type family Interpret (r :: Repr) :: Type where
    Interpret (Sum a b) = Either (Interpret a) (Interpret b)
    Interpret (Prod a b) = (Interpret a, Interpret b)
    Interpret (Cons n repr) = Interpret repr
    Interpret Unit = ()
    Interpret (Atom a) = a

class GenericA (a :: Type) where
    type Code (a ::Type) :: Repr

    from :: a -> Rep a
    to :: Rep a -> a

type Rep a = Interpret (Code a)

data SRepr (c :: Type -> Constraint) (r :: Repr) where
  SSum :: SRepr c a -> SRepr c b -> SRepr c (Sum a b)
  SProd :: SRepr c a -> SRepr c b -> SRepr c (Prod a b)
  SCons :: KnownSymbol n => Proxy n -> SRepr c a -> SRepr c (Cons n a)
  SUnit :: SRepr c Unit
  SAtom :: c a => SRepr c (Atom a)

-- generates singleton based on the type
class IsRepr (c :: Type -> Constraint) (r :: Repr) where
  repr :: SRepr c r

instance IsRepr c Unit where
  repr = SUnit

instance (IsRepr c a, KnownSymbol n) => IsRepr c (Cons n a) where
  repr = SCons (Proxy @n) repr

instance c a => IsRepr c (Atom a) where
  repr = SAtom

instance (IsRepr c a, IsRepr c b) => IsRepr c (Sum a b) where
  repr = SSum repr repr

instance (IsRepr c a, IsRepr c b) => IsRepr c (Prod a b) where
  repr = SProd repr repr

-- singleton based equality which allows us to solve problem with a single compact function rather than
-- with bloat of typeclass' instances
seq' :: SRepr Eq r -> Interpret r -> Interpret r -> Bool
seq' (SProd x y) (a, b) (c, d) = seq' x a c && seq' y b d
seq' (SSum _ _) (Left _)  (Right _) = False
seq' (SSum _ _) (Right _) (Left _) = False
seq' (SSum _ y) (Right a) (Right b) = seq' y a b
seq' (SSum x _) (Left a)  (Left b) = seq' x a b
seq' (SCons _ a) x y = seq' a x y
seq' SUnit _ _ = True
seq' SAtom a b = a == b -- enabled by Eq constraint

seq :: forall a . (GenericA a, IsRepr Eq (Code a)) => a -> a -> Bool
seq a b = seq' (repr @Eq @(Code a)) (from a) (from b)

total' :: SRepr ((~) Int) r -> Interpret r -> Int
total' SUnit _ = 0
total' SAtom a = a -- enabled by a ~ Int constraint
total' (SProd a b) (x, y) = total' a x + total' b y
total' (SSum a _) (Left x) = total' a x
total' (SSum _ b) (Right y) = total' b y
total' (SCons _ a ) x = total' a x

total :: forall a . (GenericA a, IsRepr ((~) Int) (Code a)) => a -> Int
total a = total' (repr @((~) Int) @(Code a)) (from a)

-- Family constraint on type
type family IsEnum (r :: Repr) :: Constraint where
  IsEnum (Cons n a) = a ~ Unit
  IsEnum (Sum a b)  = (IsEnum a, IsEnum b)
  IsEnum x          = TypeError (Text "provided type is not an type error")

-- nop constraint
class Top a
instance Top a

-- IsEnum brings equivalence (a :: Repr) ~ Unit thus SRep c a type to Unit and precising Interpret r to Interpret ()
enum' :: IsEnum r => SRepr Top r -> [Interpret r]
enum' (SSum a b) = (Left <$> enum' a) ++ (Right <$> enum' b)
enum' (SCons _ _) = [()]

enum :: forall a . (GenericA a, IsRepr Top (Code a), IsEnum (Code a)) => [a]
enum = to <$> enum' (repr @Top @(Code a))

eq' :: forall a . (GenericA a, GEq (Code a)) => a -> a -> Bool
eq' a b = geq @(Code a) (from a) (from b)

class GEq (r :: Repr) where
    geq :: Interpret r -> Interpret r -> Bool

instance (GEq a, GEq b) => GEq (Sum a b) where
    Left a  `geq` Left b  = geq @a a b
    Right a `geq` Right b = geq @b a b
    _       `geq`       _ = False

instance (GEq a, GEq b) => GEq (Prod a b) where
    (a, b) `geq` (c, d) = geq @a a c && geq @b b d

instance Eq a => GEq (Atom a) where
    a `geq` b = trace "whoo" $ a == b
