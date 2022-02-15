{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Leibnz where
import Conduit (Identity(runIdentity, Identity))

data EqR a b where
    Refl :: EqR a a

newtype EqL a b = EqL { cv :: forall c. c a -> c b}

rToL :: EqR a b -> EqL a b
rToL Refl = EqL id

lToR :: forall a b . EqL a b -> EqR a b
lToR (EqL f) = f @(EqR a) Refl

castIt :: EqL a b -> a -> b
castIt (EqL f) = runIdentity . f . Identity

newtype To c b a = To { unTo :: c a -> c b }

symm :: forall a b . EqL a b -> EqL b a
symm (EqL f) = EqL tp
    where tp :: forall c . c b -> c a
          tp = unTo . f @(To c a) . To $ (id :: c a -> c a)

refl :: EqL a a
refl = EqL id

newtype Symm a b = Symm { unSym :: EqL b a }

symm' :: EqL a b -> EqL b a
symm' (EqL f) = unSym . f . Symm $ refl

trans :: EqL a b -> EqL b c -> EqL a c
trans (EqL ab) (EqL bc) = EqL (bc . ab)

trans' :: forall a b c . EqL a b -> EqL b c -> EqL a c
trans' ab bc = cv bc @(EqL a) ab

newtype LiftEq f a b = LiftEq { unLift :: EqL (f a) (f b) } 

gen :: forall f a b . EqL a b -> EqL (f a) (f b)
gen (EqL p) = unLift . p . LiftEq $ refl @(f a)

type DeApp :: j -> k
type family DeApp fa where
    DeApp (f a) = a

newtype UnliftEq a fa = UnliftEq { unliftEq :: EqL a (DeApp fa) }

decs :: forall f a b . EqL (f a) (f b) -> EqL a b 
decs (EqL p) = unliftEq . p . UnliftEq $ refl @a