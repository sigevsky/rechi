{-# LANGUAGE DataKinds, GADTs, TypeOperators, PolyKinds #-}

module SIngs where
import Data.Type.Equality
import Unsafe.Coerce ( unsafeCoerce )

data Nat = Z | S Nat

data SNat (n :: Nat) where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)

instance TestEquality SNat where
    testEquality SZ SZ = Just Refl
    testEquality SZ _ = Nothing
    testEquality _ SZ = Nothing
    testEquality (SS a) (SS b) = 
        case testEquality a b of 
            Nothing -> Nothing 
            Just Refl -> Just Refl

type family TEq a b :: Bool where
    a `TEq` a = True
    _ `TEq` _ = False

tEq :: TestEquality f => f a -> f b -> SBool (a `TEq` b)
tEq a b = case testEquality a b of 
            Just Refl -> STrue
            Nothing  -> unsafeCoerce SFalse

data SBool (b :: Bool) where
    STrue :: SBool True 
    SFalse :: SBool False

type family (n :: Nat) :< (m :: Nat) :: Bool where
    n :< Z = False
    Z :< m = True
    (S a) :< (S b) = a :< b

-- leq :: SNat n -> SNat m -> SBool (n :< m)
-- leq _ SZ = SFalse
-- leq SZ _ = STrue
-- leq (SS a) (SS b) = leq a b
prev :: SNat (S a) -> SNat a
prev (SS a) = a

-- leq :: SNat n -> SNat m -> SBool (n :< m)
-- leq n m = case (m `tEq` SZ, n `tEq` SZ) of
--             (STrue, SFalse) -> STrue
--             (SFalse, STrue) -> SFalse
--             (SFalse, SFalse) -> let b = prev n in _