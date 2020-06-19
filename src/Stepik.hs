{-# LANGUAGE TypeOperators #-}

module Stepik
    ()
where

import Control.Applicative hiding (many)
import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

anyChr :: Prs Char
anyChr = Prs h
        where h [] = Nothing
              h (x:xs) = Just (x, xs)

many :: Prs a -> Prs [a]
many p = (:) <$> p <*> many p <|> pure [] 

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p <|> empty 

char :: Char -> Prs Char
char c = Prs h
    where h [] = Nothing
          h (x:xs) | c == x = Just (x, xs)
                   | otherwise = Nothing

digit :: Prs Char
digit = Prs h
    where h [] = Nothing
          h (x:xs) | isDigit x = Just $ (x, xs)
                   | otherwise = Nothing

digits :: Prs String
digits = many1 digit

nat :: Prs Int
nat = read <$> many1 digit

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

instance Functor Prs where
    fmap f (Prs g) = Prs h
        where h s = case g s of
                        Just (a, rest) -> Just (f a, rest)
                        Nothing -> Nothing

instance Applicative Prs where
    pure a = Prs $ \s -> Just (a, s)
    (Prs f) <*> (Prs fa) = Prs h 
        where h s = do
                (g, rest') <- f s
                (r, rest'') <- fa rest'
                pure (g r, rest'')

instance Alternative Prs where
    empty = Prs $ const Nothing
    (Prs fa) <|> (Prs fb) = Prs h
        where h s = case fa s of
                        Just n -> Just n
                        Nothing -> case fb s of
                            Just n -> Just n
                            Nothing -> Nothing



newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
    fmap f (PrsE g) = PrsE h
        where h s = do
                (r, rest') <- g s
                pure (f r, rest')

instance Applicative PrsE where
    pure a = PrsE $ \s -> Right (a, s)
    (PrsE f) <*> (PrsE fa) = PrsE h 
        where h s = do
                (g, rest') <- f s
                (r, rest'') <- fa rest'
                pure (g r, rest'')

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE h
        where h [] = Left "unexpected end of input"
              h (x:xs) | pr x = Right (x, xs)
                       | otherwise = Left $ "unexpected " ++ [x] 

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a)}

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (2, ('c', True))

b :: B t
b = Cmps (True, id, Right 2)

c :: C
c  = Cmps (const id)

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show)

instance (Functor f, Functor g, Functor h) =>
  Functor (Cmps3 f g h) where
    fmap fe (Cmps3 r) = Cmps3 $ (fmap . fmap . fmap $ fe) r

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap unCmps3 . getCmps
