-- https://williamyaoh.com/posts/2019-04-25-lens-exercises.html
{-# LANGUAGE TemplateHaskell, NamedFieldPuns, QuasiQuotes, TupleSections, UndecidableInstances, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module LensSandbox where

import Data.Text
import qualified Data.Text.IO as TIO
import Control.Lens
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Lens
import Data.Monoid (First(..), Endo (..))
import Control.Arrow ((&&&))
import Control.Monad.Reader
import qualified Data.Vector as V
import Control.Lens.Action
import Control.Lens.Action.Internal
import Control.Applicative (liftA2)
import Data.Tagged

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

a1 = user1 ^. metadata . associatedIPs
a2 = user1 & metadata . associatedIPs %~ Prelude.reverse
a3 = user1 & name %~ Data.Text.toTitle
a4 = user1 & metadata . numLogins .~ 0
a5 = user1 & metadata . associatedIPs %~ (: []) . Prelude.head

set' :: ((a -> Identity b) -> s -> Identity t)
     -> b
     -> s
     -> t
set' l b = runIdentity . l (Identity . const b)

over' :: ((a -> Identity b) -> s -> Identity t)
      -> (a -> b)
      -> s
      -> t
over' l f = runIdentity . l (Identity . f)

get' :: s
     -> ((a -> Const a b) -> s -> Const a t)
     -> a
get' s f = getConst $ f Const s

nameL :: Functor f => (Text -> f Text) -> User -> f User
nameL f u@User{_name = nm}= newUser <$> f nm
    where newUser nn = u {_name = nn }

-- lenses composition ordrer
-- aL :: Functor f => (a -> f b) -> s -> f t
-- bL :: Functor f => (c -> f d) -> a -> f b
-- => aL . bL ~> (c -> f d) -> s -> f t

user2 = [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

user3 = [aesonQQ|
  {
    "name": "ye.xiu",
    "metadata": {
      "num_logins": 27
    }
  }
|]

b1 = user3 & key "metadata" . key "num_logins" . _Integer %~ (+ 1)
b2 = user2 ^? key "email"
b3 = user2 & key "name" . _String .~ "50"


cond' :: s
     -> ((a -> Const (First a) b) -> s -> Const (First a) t)
     -> Maybe a
cond' s f = getFirst . getConst $ f (Const . First . Just) s

b2' = user2 `cond'` key "email"

_Just' :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
_Just' f Nothing = pure Nothing
_Just' f (Just a) = Just <$> f a


users = [aesonQQ|
  {
    "users": [
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com",
        "metadata": {
          "num_logins": 5
        }
      },
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27,
          "associated_ips": [
            "52.49.1.233",
            "52.49.1.234"
          ]
        }
      },
      {
        "name": "su.mucheng",
        "email": "smucheng@xingxin.com",
        "metadata": {
          "associated_ips": [
            "51.2.244.193"
          ]
        }
      }
    ]
  }
|]

-- ^..
th' :: s -> Getting (Endo [a]) s a -> [a]
th' s g = flip appEndo [] . getConst $ g (Const . Endo . (:)) s

c1 = users ^.. key "users" . values . key "name" . _String . filtered (\name -> Data.Text.head name == 's')

c2 = users ^.. key "users" . values . filtered hasApi
  where hasApi v = (key "metadata" . key "associated_ips" . values . _String . filtered (== "51.2.244.193")) `has` v 

c3 = users & 
  traverseOf_ 
    (key "users" . values . key "metadata" . key "associated_ips" . values . _String) 
    TIO.putStrLn

c5 = users & 
  traverseOf 
    (key "users" . values . key "name" . _String) 
    (\name -> TIO.putStrLn ("Prefix for: " <> name) >> TIO.getLine >>= \prefix -> return $ prefix <> "_" <> name) 

-- t = users &
--      sequenceAOf
--        (key "users" . values . key "associated_ips" . _Array)


-- https://artyom.me/lens-over-tea-1

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (,x) <$> f a 

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x,) <$> f a

-- Make a lens out of a getter and a setter.
lens' :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens' get set f s =  set s <$> f (get s)

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f (Left a) = Left <$> l1 f a
choosing l1 l2 f (Right a) = Right <$> l2 f a

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (id &&& f)

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united = lens (const ()) const

_all :: Eq a => a -> Lens' [a] a
_all i = lens (const i) setter
  where setter l v = fmap (\x -> if x == i then v else x) l

_all' :: Eq a => a -> Traversal' [a] a
_all' ref f = traverse update
  where
    update old = if old == ref then f old else pure old

view' :: MonadReader s m => Getting a s a -> m a
view' l = asks $ getConst . l Const

act' :: (s -> m a) -> Action m s a
act' f eg s = effective $ f s >>= ineffective . eg

coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = contramap (const ()) . fmap (const ())

tg :: (s -> a) -> Getter s a
tg f ag s = coerce $ ag (f s)

-- type Fold s a = forall f . (Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

newtype Folding' f a = Folding' { getFold :: f a }

instance (Contravariant f, Applicative f) => Semigroup (Folding' f a) where
  (Folding' fa) <> (Folding' fb) = Folding' (fa *> fb)

instance (Contravariant f, Applicative f) => Monoid (Folding' f a) where
  mempty = Folding' (coerce $ pure ())

folded' :: Foldable t => Fold (t a) a
folded' af s = coerce $ getFold $ foldMap (Folding' . af) s

bothF :: (Contravariant f, Applicative f) => (a -> f b) -> (a, a) -> f (b, b)
bothF f (a, b) = (,) <$> f a <*> f b

data Echange a b s t = Echange { from :: s -> a, to :: b -> t }

instance Functor (Echange a b s) where
  fmap g (Echange f t) = Echange f (g . t)


instance Profunctor (Echange a b) where
  lmap ab (Echange f t) = Echange (f . ab) t
  rmap bc (Echange f t) = Echange f (bc . t)

type LaIso a b s t = Echange a b a (Identity b) -> Echange a b s (Identity t)

-- extract functions from an isomorphism

withIso1 :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso1 iso k = case iso (Echange id Identity) of
                    Echange sa bt -> k sa (runIdentity . bt)

withIso2 :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso2 iso k = k sa bt
  where sa s = getConst $ iso Const s
        bt b = runIdentity . unTagged $ iso (Tagged . Identity $ b)


-- alternative s -> a extraction without Const
-- Input is sort of dual to Tagged but operates on inputs
newtype Input r a b = Input { runInput :: a -> r }

instance Profunctor (Input r) where
  dimap i _ (Input ar) = Input (ar . i)


withIso3 :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso3 iso k = k sa bt
  where sa = runInput (iso (Input id :: Input a a (Identity b)))
        bt b = runIdentity . unTagged $ iso (Tagged . Identity $ b)


class Bypass p where
  bypass :: (s -> Either a t) -> p a t -> p s t

-- p a b -> p (Either a c) (Either b c)
-- Either a c -> Either a (Either b c)

instance Bypass Tagged where
  bypass _ = retag

class Profunctor p => Choice' p where
  left''  :: p a b -> p (Either a c) (Either b c)
  right'' :: p a b -> p (Either c a) (Either c b)
  right'' = dimap swap swap . left''
    where swap (Left a) = Right a
          swap (Right a) = Left a

-- UndecidableInstances
instance Choice p => Bypass p where
  bypass f = dimap f squash . left'
    where squash (Left a) = a
          squash (Right a) = a

instance (Profunctor p, Bypass p) => Choice' p where
  left'' = bypass dist . rmap Left
    where dist (Left a) = Left a
          dist (Right a) = Right . Right $ a

instance Bypass (->) where
  bypass f ab = \s -> case f s of
                        Left a -> ab a
                        Right b -> b


prismC :: (s -> Either a t) -> (b -> t) -> Prism s t a b
prismC sEi bt = adj . left'
  where
    adj = dimap 
      (fmap pure . sEi)
      (either (fmap bt) id)

prismC' :: (s -> Maybe a) -> (b -> s) -> Prism s s a b
prismC' smb = prismC remap
  where remap s | Nothing <- smb s = Right s
        remap s | Just a <- smb s  = Left a

withPrism :: Prism s t a b -> ((s -> Either a t) -> (b -> t) -> r) -> r
withPrism psm k = case psm (Market' Left Identity) of
                  Market' a b -> k (fmap runIdentity . a) (runIdentity . b)

data Market' a b s t = Market' (s -> Either a t) (b -> t)

instance Profunctor (Market' a b) where
  lmap r (Market' f g) = Market' (f . r) g
  rmap r (Market' f g) = Market' remap (r . g)
    where remap s | Left a  <- f s = Left a
                  | Right b <- f s = Right (r b)

instance Choice (Market' a b) where
  left' :: Market' a b s t -> Market' a b (Either s c) (Either t c)
  left' (Market' f g) = Market' in' out'
    where in' (Left s) | (Left a) <- f s = Left a
          in' (Left s) | (Right t) <- f s = Right . Left $ t
          in' (Right c) = Right . Right $ c
          out' b = Left $ g b


ats :: Eq a => [a] -> Traversal' [a] a
ats _ _ [] = pure []
ats ixs f (x: xs) | x `elem` ixs = (:) <$> f x <*> ats ixs f xs 
ats ixs f (x: xs) = (x:) <$> ats ixs f xs

ixs :: Eq a => [a] -> Traversal' [a] a
ixs ixs f (x: xs) | x `elem` ixs = (:) <$> f x <*> ats ixs f xs 
ixs ixs f (x: xs) = (x:) <$> ats ixs f xs
ixs _ _ [] = pure []
        