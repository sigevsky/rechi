{-# LANGUAGE DeriveFunctor #-}

module MonadicParse
 ()
where

import           Control.Applicative
import           Data.List
import           Data.Char

data Consumed a = Empty (Status a) | Consumed (Status a)
    deriving (Functor, Show)

data Status a = Ok a String | Error deriving (Functor, Show)

newtype Parser a = Parser { run :: String -> Consumed a }
    deriving Functor


instance Applicative Parser where
    pure a = Parser $ \input -> Empty (Ok a input)
    (Parser pf) <*> (Parser ps) = Parser pc
        where pc input = case pf input of 
                            Empty (Ok fa rest) -> 
                                case ps rest of 
                                    Empty (Ok a rest2) -> Empty (Ok (fa a) rest2)
                                    Empty Error -> Empty Error
                                    Consumed (Ok a rest2) -> Consumed (Ok (fa a) rest2)
                                    Consumed Error -> Consumed Error
                            Consumed (Ok fa rest) -> 
                                case ps rest of 
                                    Empty (Ok a rest2) -> Consumed (Ok (fa a) rest2)
                                    Empty Error -> Consumed Error
                                    Consumed (Ok a rest2) -> Consumed (Ok (fa a) rest2)
                                    Consumed Error -> Consumed Error

instance Monad Parser where
    return = pure
    (Parser ps) >>= pf = Parser pc
        where pc input = case ps input of 
                            Empty (Ok a rest) -> run (pf a) rest
                            Empty Error -> Empty Error
                            Consumed (Ok a rest) -> 
                                Consumed $ case run (pf a) rest of
                                    Empty e -> e
                                    Consumed e -> e

                                

satisfy :: (Char -> Bool) -> Parser Char
satisfy test = Parser $ \case 
                        (s:sx) | test s -> Consumed (Ok s sx)
                        _ -> Empty Error

char c = satisfy (==c)
letter = satisfy isAlpha
digit = satisfy isDigit

parseString :: String -> Parser String
parseString temp = Parser f
  where
    f source | temp `isPrefixOf` source = Consumed $ Ok temp (drop (length temp) source)
    f source                            = Empty Error 


data HelloWorld = HW String String deriving Show

hello = parseString "hello"
world = parseString "world"
helloWorld = HW <$> hello <*> world

instance Alternative Parser where
    empty = Parser $ \input -> Empty Error
    (Parser pf) <|> (Parser pg) = Parser ph
        where ph input = case pf input of
                            Empty Error -> pg input
                            Empty a -> case pg input of
                                Empty _ -> Empty a
                                consumed -> consumed
                            consumed -> consumed

skip :: String -> Parser ()
skip [] = return ()
skip (s:st) = char s >> skip st

many1 p = do
    x  <- p
    xs <- many1 p <|> pure []
    return (x: xs)


data Method = GET | POST | PUT | HEAD | OPTION | PATCH

data Header = ContentType String | Accept String | Other String String 

data HttpReq = HttpHeader {
    method :: Method,
    headers :: [Header]
}

-- LL(1) vs LL(inf)

oneOf = skip "peka" <|> skip "poda"

-- Converts LL(1) parser to LL(inf) i.e introduces backtracking while also screwing error messages
try :: Parser a -> Parser a
try (Parser p) = Parser $ \input -> case p input of
                    Consumed Error -> Empty Error
                    other -> other


oneOfEnchanced = try (skip "peka") <|> skip "poda"

