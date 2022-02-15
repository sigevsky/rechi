module CondExpr where

import Conduit
import Control.Applicative (Applicative(liftA2))

data Expr
    = Lit Int
    | Hole
    | Add Expr Expr
    | Mult Expr Expr


expr :: Expr
expr = Add Hole (Mult Hole (Lit 2))

eval :: Expr -> ConduitT Int Void IO (Either String Int)
eval (Lit i) = pure . Right $ i
eval Hole = await >>= \mbI ->
    pure $ maybe (Left "supplied not enough elements") Right mbI
eval (Mult a b) = do
    eiA <- eval a
    eiB <- eval b
    return $ liftA2 (+) eiA eiB

eval (Add a b) = do
    eiA <- eval a
    eiB <- eval b
    return $ liftA2 (+) eiA eiB

main :: IO (Either String Int)
main = runConduit (yieldMany [1 :: Int, 2] .| eval expr)