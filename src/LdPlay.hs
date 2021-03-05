module LdPlay where

import Control.Lens
import Data.Function (fix)


data Expr = I Int
 | S String
 | Lam String Expr
 | App Expr Expr
 | Sum Expr Expr deriving Show

tvExpr :: Traversal' Expr Expr
tvExpr f (I n) = pure (I n)
tvExpr f (S s) = pure (S s)
tvExpr f (Lam x ex) = Lam x <$> f ex
tvExpr f (App a b) = App <$> f a <*> f b
tvExpr f (Sum a b) = Sum <$> f a <*> f b

reduceSum :: Expr -> Expr
reduceSum (Sum (I a) (I b)) = I (a + b)
reduceSum x = x

itv :: Traversal a b a b -> (b -> b) -> (a -> b)
itv l f = fix (\rc -> f . over l rc)

tvDeep = itv tvExpr reduceSum

simpleE = Lam "x" $ Sum (I 2) (I 4)
