{-# LANGUAGE DataKinds, GADTs #-}
module DoorKinds
  ()
where

import Data.Kind (Type)


data DoorState = Open | Closed | Locked

newtype Door (s :: DoorState) = MkDoor String deriving Show

close :: Door Open -> Door Closed
close (MkDoor a) = MkDoor a

class LockDoor (a :: DoorState) where
  lock :: Door a -> Door Locked

instance LockDoor Open where
  lock = lock . close

instance LockDoor Closed where
  lock (MkDoor s) = MkDoor s

instance LockDoor Locked where
  lock (MkDoor s) = MkDoor s

data SingD (s :: DoorState) :: Type where
  SOpen :: SingD Open
  SClosed :: SingD Closed
  SLocked :: SingD Locked

-- We pass `SingD s` here instead of `Door s` since the former carries 3 options by which we can pattern match
-- obtaining concrete constraints on s ~ (a :: DoorState)
fromString :: String -> (forall s. SingD s -> Door s -> r) -> r
fromString "O" f =  f SOpen   (MkDoor "wood")
fromString "C" f =  f SClosed (MkDoor "wood")
fromString "L" f =  f SLocked (MkDoor "wood")

rd :: SingD s -> Door s -> Door Locked
rd SOpen   = lock
rd SClosed = lock
rd SLocked = lock
