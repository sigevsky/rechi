{-# LANGUAGE RankNTypes #-}

module ScottEtChurch
  ()
where

data List a = Nil | Cons a (List a)

unconsList :: (a -> List a -> r) -> r -> List a -> r
unconsList _ r Nil = r
unconsList f _ (Cons x xs) = f x xs

printL :: Show a => List a -> String
printL = unconsList sh nil
           where nil = ""
                 sh x xs = show x <> " " <> unconsList sh nil xs


-- Scott encoding ( via func representation on how list is pattern matched via continuations )

newtype ListS a = ListS {
  uncons :: forall r . (a -> ListS a -> r) -> r -> r
}

nilS :: ListS a
nilS = ListS (\_ ni -> ni)

consS :: a -> ListS a -> ListS a
consS a as = ListS (\cn _ -> cn a as)

oneS :: ListS Integer
oneS = ListS (\co _ -> co 1 nilS)

toScott :: [a] -> ListS a
toScott [] = ListS (\_ ni -> ni)
toScott (x : xs) = ListS (\co _ -> co x (toScott xs))


sumS :: Num a => ListS a -> a
sumS (ListS f) = f cons ni
      where ni = 0
            cons x xs = x + sumS xs

testScottSum = show . sumS . toScott $ [0, 1, 2, 3, 4]

instance Functor ListS where
  fmap g (ListS f') = ListS $ \co ni -> f' (\a lsa -> co (g a) (fmap g lsa)) ni

-- Church list encoding ( via fold represantation )

newtype ListF a = ListF { fd :: forall r . (a -> r -> r) -> r -> r }

nilF :: ListF a
nilF = ListF $ \_ ni -> ni

consF :: a -> ListF a -> ListF a
consF x (ListF f) = ListF $ \con ni -> con x (f con ni)


unconsF :: (a -> r -> r) -> r -> ListF a -> r
unconsF con ni (ListF f) = f con ni

churchList = consF 1 (consF 2 (consF 3 nilF))

sumF :: Num a => ListF a -> a
sumF = unconsF (+) 0

test1 = sumF churchList

instance Functor ListF where
  fmap g (ListF f') = ListF $ \acc ni -> f' (acc . g) ni
