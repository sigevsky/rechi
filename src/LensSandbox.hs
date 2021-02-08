-- https://williamyaoh.com/posts/2019-04-25-lens-exercises.html
{-# LANGUAGE TemplateHaskell, NamedFieldPuns, QuasiQuotes #-}

module LensSandbox where

import Data.Text
import Control.Lens
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Lens
import Data.Monoid (First(..))

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


