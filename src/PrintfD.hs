{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrintfD
    ()
where

import Data.Kind (Type)
import GHC.TypeLits
import Data.Proxy

data (:<<) (a :: l) (b :: r)

infixr 5 :<<

class HasPrintf a where
    type Print a :: Type
    format :: String -> Proxy a -> Print a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
    type Print text = String
    format acc _ = acc <> symbolVal (Proxy @text) 

instance (KnownSymbol text, HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where
    type Print ((text :: Symbol) :<< a) = Print a
    format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (Show param, HasPrintf a) => HasPrintf ((param :: Type) :<< a) where
    type Print ((param :: Type) :<< a) = param -> Print a
    format s _ param = format (s <> show param) (Proxy @a)

printf :: (HasPrintf a) => Proxy a -> Print a
printf = format ""

printfa :: forall a . (HasPrintf a) => Print a
printfa = format "" (Proxy @a)

-- :t p
p :: Proxy (Print ("Event " :<< String :<< " happend " :<< Int :<< " times!"))
p = Proxy

testStr :: String
testStr = printf (Proxy @("Event " :<< String :<< " happend " :<< Int :<< " times!")) "boom" 2

testAppStr :: String
testAppStr = printfa @(String :<< "world") "hello"