module NaiveGuessANum
    ()
where

import           System.Random
import           Control.Monad
import qualified Text.Read                     as T
import           Control.Monad.State

-- Language support

data Language = Language { greetings :: String
                         , letsplay :: String -> String
                         , guessNumber :: String
                         , correct :: String -> String
                         , wrong :: Int -> String
                         }

enLang :: Language
enLang = Language
    "Hello stranger, tell me your name"
    (\name -> "Lets play the game, " <> name)
    "Guess a number from 1 to 7"
    (\name ->
        "Nice try "
            <> name
            <> " You guessed correctly! Do you want to try again?"
    )
    (\n -> "Not " <> show n <> "!")

frLang :: Language
frLang = Language
    "Bonjour vagabond, dit moi ton nom"
    (\name -> "Jouons à un jeu, " <> name)
    "Devinez un nombre de 1 à 7"
    (\name ->
        "Bien tenté "
            <> name
            <> " T'a bien deviné ! Vuex tu essayer de nouveau?"
    )
    (\n -> "C'est pas " <> show n <> "!")

-- Actual Programm

class Console m where
    printStr :: String -> m ()
    readStr :: m String

class Rnd m where
    nextInt :: m Int

readNum :: (Console m, Monad m) => m (Maybe Int)
readNum = T.readMaybe <$> readStr

gameLoop :: (Monad m, Console m, Rnd m) => String -> Language -> m ()
gameLoop name lang = do
    mguess <- readNum
    case mguess of
        Nothing    -> gameLoop name lang
        Just guess -> do
            num <- nextInt
            let res = num == guess
            if res
                then do
                    _ <-
                        printStr $ correct lang name 
                    answ <- readStr
                    when (answ == "y") $ gameLoop name lang
                else printStr (wrong lang guess) *> gameLoop name lang

game :: (Monad m, Console m, Rnd m) => Language -> m ()
game lang = do
    _    <- printStr $ greetings lang
    name <- readStr
    printStr (letsplay lang name)
        *> printStr (guessNumber lang)
        *> gameLoop name lang

-- Interacting with outer world

instance Rnd IO where
    nextInt = randomRIO (1, 5)

instance Console IO where
    printStr = putStrLn
    readStr  = getLine

start :: IO ()
start = game frLang


-- Testing 

data TestData = TestData { inputs :: [String]
                         , outputs :: [String]
                         , guesses :: [Int]
                         } deriving Show

instance Console (State TestData) where
    printStr output = do
        TestData i o g <- get
        put $ TestData i (output : o) g

    readStr = do
        TestData i o g <- get
        _              <- put $ TestData (drop 1 i) o g
        return (head i)

instance Rnd (State TestData) where
    nextInt = do
        TestData i o g <- get
        _              <- put $ TestData i o (drop 1 g)
        return (head g)

testData :: TestData
testData = TestData ["Nikita", "1", "2", "3", "n"] [] [3, 3, 3]

runTest :: TestData
runTest = execState (game frLang) testData
