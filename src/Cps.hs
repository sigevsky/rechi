module Cps
  ()
where

-- cps :: b -> (a -> r) -> r ~ b -> a function in continuation-passing form

addCps :: Num a => a -> a -> ((a -> r) -> r)
addCps x y k = k $ x + y

squareCps :: Num a => a -> ((a -> r) -> r)
squareCps x k = k $ x * x

poly :: Num a => a -> a -> ((a -> r) -> r)
poly x y k = squareCps x $ \xs -> squareCps y $ \ys -> addCps xs ys k

comb :: (b -> (c -> r) -> r) -> (a -> (b -> r) -> r) -> a -> (c -> r) -> r
comb f g a cr = g a (\b -> f b cr)

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f k = s $ \a -> f a k

add1AndSquare :: Num a => a -> (a -> r) -> r
add1AndSquare = squareCps `comb` addCps 1

test = add1AndSquare 1 print

-- when there are nested lambdas using forall r., this type fails to be instantiated due
-- to the fact that in outter lambda r bounds to some fixed type t while nested one requers rigid type r!
newtype Cont a = Cont { runCont :: forall r . (a -> r) -> r }


instance Functor Cont where
    fmap g (Cont f) = Cont $ \br -> f $ \a -> br $ g a

instance Applicative Cont where
    pure a = Cont $ \k -> k a
    (Cont z) <*> (Cont f) = Cont $ \br -> f $ \a -> z $ \ab -> br . ab $ a

instance Monad Cont where
    (Cont f) >>= g = Cont $ \br -> f $ \a -> runCont (g a) br


squareCt :: Num a => a -> Cont a
squareCt a = pure (a * a)

addCt :: Num a => a -> a -> Cont a
addCt x y = pure (x + y)

polyCt :: Num a => a -> a -> Cont a
polyCt x y = do
    xs <- squareCt x
    ys <- squareCt y
    addCt xs ys

test2 = runCont (polyCt 2 3) print

-- fails to unify r1 and r because of forall r. definition in Cont type declaration
-- callCC :: ((a -> Cont a) -> Cont a) -> Cont a
-- callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h