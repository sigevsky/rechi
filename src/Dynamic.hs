{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Dynamic
    ()
where

import Data.Typeable
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

data Dynamic where
    Dynamic :: (Typeable a, Show a) => a -> Dynamic

elimDynamic :: (forall a . Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

showDynamic :: (forall a . Show a => a -> r) -> Dynamic -> r
showDynamic f (Dynamic a) = f a

fromDynamic :: (Typeable b) =>  Dynamic -> Maybe b
fromDynamic = elimDynamic cast

lift2D :: (Typeable a, Typeable b, Typeable r, Show a, Show b, Show r) =>
    (a -> b -> r) -> Dynamic -> Dynamic -> Maybe Dynamic
lift2D f a b = fmap Dynamic foo
    where foo = f <$> fromDynamic a <*> fromDynamic b


pyPlus :: Dynamic -> Dynamic -> Maybe Dynamic
pyPlus a b = asum actions
    where actions = [ lift2D @Int @Int (+) a b
                    , lift2D @String @String (++) a b
                    , lift2D @Int @String (\i s -> show i <> s) a b
                    , lift2D @String @String (\s i -> s <> show i) a b
                    ]

testCorrectAddition = maybe "failed to add values" (showDynamic show) tc
    where tc = Dynamic @Int 1 `pyPlus` Dynamic @String "hello world"
testInCorrectAddition = maybe "failed to add values" (showDynamic show) tc
    where tc = Dynamic @(Maybe Int) (Just 1) `pyPlus` Dynamic @String "hello world"