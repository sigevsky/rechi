{-# LANGUAGE TupleSections #-}

module State (

) where

import Control.Monad

newtype State s a = State { run :: s -> (s, a) }

instance Functor (State s) where
    fmap f (State t) = State p
        where p s = let { (st, val) = t s } in (st, f val)

instance Applicative (State s) where
    pure a = State (, a)
    (State f) <*> (State t) = State p
        where p s = let (s1, g) = f s                                   
                        (s2, res) = t s1                    
                    in (s2, g res)
                    
instance Monad (State s) where
    (State t) >>= f = State p
        where p s = let (st, val) = t s
                        ns = f val
                    in run ns st

getState :: State s a -> State s s
getState (State run) = State $ \s -> let (s1, a) = run s in (s1, s1)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put val = State $ \s -> (val, ())

sequenceGen :: Int -> State Int Int
sequenceGen a = State $ \s -> (s + a, s + a)

example :: State Int Int 
example = do 
    a <- sequenceGen 5
    b <- sequenceGen 6
    return $ a + b
