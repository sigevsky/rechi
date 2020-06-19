module Lens
    ()
where

import Control.Comonad.Store
import Data.Functor.Identity
import Data.Functor.Const
import Data.Char

type Lens a b = a -> Store b a

view :: Lens a b -> a -> b
view lens a = let (StoreT _ s) = lens a in s

set :: Lens a b -> a -> b -> a
set lens a = let (StoreT (Identity v) s) = lens a in v

update :: Lens a b -> (b -> b) -> a -> a
update lens r a = let (StoreT (Identity v) oldS) = lens a in v (r oldS)

-- Tets

toUpperStr :: String -> String 
toUpperStr a = toUpper <$> a

data Person = Person { name :: String, surname :: String } deriving Show

nameL :: Lens Person String
nameL p = store (\newName -> p { name = newName }) (name p)

test = view nameL . update nameL toUpperStr $ Person "Mykyta" "Levashov"

-- van
fmapDefault f = runIdentity . traverse (Identity . f)

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t 
over l f = runIdentity . l (Identity . f)

type Setter s t a b = (a -> Identity b) -> s -> Identity t

sets :: ((a -> b) -> s -> t) -> Setter s t a b 
sets g f' = Identity . g (runIdentity . f') 

-- over . sets = sets . over = id

mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

fmap' :: Functor f => (a -> b) -> f a -> f b
fmap' = over mapped

fmap'' :: (Traversable f) => (a -> b) -> f a -> f b
fmap'' = over traverse


doubleMapped :: (Functor f, Functor g) => Setter (f (g a)) (f (g b)) a b
doubleMapped = mapped . mapped

-- data Const a b = Const a

foldMapDefault :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMapDefault f = getConst . traverse (Const . f)

foldMapOf :: ((a -> Const r e) -> s -> Const r k) -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)

type Getter r s a = ((a -> Const r a) -> s -> Const r s)

folds :: ((a -> r) -> s -> r) -> Getter r s a
folds g f' = Const . g (getConst . f')

-- folds . foldMapOf = foldMapOf . folds = id 

(.~) :: Setter s t a b -> b -> s -> t
l .~ d = runIdentity . l (Identity . const d) 

rename :: Setter Person Person String String
rename = sets $ \f (Person name surname) -> Person (f name) surname

upperSurname :: Getter String Person String
upperSurname = folds $ \f (Person name surname) -> f surname

test1 = rename .~ "Boudin" $ Person "Mykyta" "Anreevich"
test2 = upperSurname

