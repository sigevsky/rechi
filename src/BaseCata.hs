{-# LANGUAGE DeriveFunctor #-}
module BaseCata
  ()
where

newtype Fix f = Fx (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fx f) = alg (cata alg <$> f) 


data TreeF a b = Leaf a | Branch b b deriving Functor 
leaf = Fx . Leaf
branch a b = Fx $ Branch a b

simpleTree = branch (branch (leaf 1) (leaf 3)) (leaf 2)

sumStep :: Num a => TreeF a a -> a
sumStep (Leaf a) = a
sumStep (Branch a b) = a + b

showStep :: TreeF Integer String -> String
showStep (Leaf s) = "Leaf(" <> show s <> ")"
showStep (Branch a b) = "Branch(left: " <> a <> ", right: " <> b <> ")"

sumT = cata sumStep

showT = cata showStep