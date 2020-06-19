module Parsers
    ()
where

import           Control.Applicative
import           Data.List
import           Data.Char

newtype Parser a = Parser { p :: String -> [(String, a)] }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> (fmap . fmap) f (p s) -- map relations in 1 [] 2 ()

instance Applicative Parser where
    pure v = Parser $ \s -> [(s, v)]
    Parser f <*> Parser p = Parser c
        where c s = [ (res, h t) | (s1, h) <- f s, (res, t) <- p s1 ] -- h :: a -> b

instance Alternative Parser where
    empty = Parser (const [])
    (Parser px) <|> (Parser py) = Parser (\s -> px s ++ py s)

tryParse :: String -> Parser a -> Maybe a
tryParse s (Parser p) = case p s of
    [("", v)] -> Just v
    _         -> Nothing

-- Simple Parsers 

parseP :: (Char -> Bool) -> Parser String
parseP p = Parser f
  where
    f [] = []
    f a@(x : xs) | p x       = [(xs, [x])]
                 | otherwise = []

charP :: Char -> Parser String
charP c = parseP (== c) 

skip :: (Char -> Bool) -> Parser ()
skip p = Parser f where f s = [(dropWhile p s, ())]

digitParser :: Parser Int
digitParser = Parser f
  where
    f (x : xs) | isDigit x = [(xs, read [x])]
    f _                    = []

parseString :: String -> Parser String
parseString temp = Parser f
  where
    f source | temp `isPrefixOf` source = [(drop (length temp) source, temp)]
    f source                            = []

data HelloWorld = HW String String deriving Show

hello = parseString "hello"
world = parseString "world"
helloWorld = pure HW <*> hello <*> world
skipSpaces = skip (== ' ')

-- pq problem (or rather its absense for applicative parsers)

pqParser = parseString "pq" <|> liftA2 (<>) (charP 'p') (parseString "pq") 
loopingPqParser = liftA2 (<>) (charP 'p' <|> pure "s") (parseString "pq")

skipM :: String -> Parser ()
skipM [] = pure ()
skipM (s:st) = charP s *> skipM st

skipOneOf = skipM "peka" <|> skipM "poda"

oneOf = parseString "peka" <|> parseString "poda"
