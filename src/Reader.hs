{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MonoLocalBinds         #-}

module Reader
    ()
where

newtype ReaderT e m a = ReaderT { runReader :: e -> m a }

instance Functor m => Functor (ReaderT e m) where
    fmap g fa = ReaderT (fmap g . runReader fa)

instance Applicative m => Applicative (ReaderT e m) where
    pure a   = ReaderT (\_ -> pure a) 
    g <*> fa = ReaderT $ \e -> runReader g e <*> runReader fa e

instance Monad m => Monad (ReaderT e m) where
    fa >>= g = ReaderT foo
        where foo e = ma >>= amb
               where ma    = runReader fa e
                     amb a = runReader (g a) e

class MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance Applicative m => MonadReader e (ReaderT e m) where
    ask = ReaderT $ \e -> pure e
    local f ma = ReaderT $ \e -> runReader ma (f e)

class MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO (ReaderT e IO) where
    liftIO io = ReaderT $ \_ -> io


-- Test reader

class Console m where
    readLine :: m String
    printLine :: String -> m ()


instance MonadIO m => Console m where
    readLine    = liftIO getLine
    printLine s = liftIO $ putStrLn s

data Hotel = Hotel { hotelName :: String }

sayHelloToGentelmen :: (MonadReader Hotel m, Console m, Monad m) => m ()
sayHelloToGentelmen = do
    name  <- readLine
    hotel <- ask
    printLine $ name <> ", welcome" <> " to our lovely hotel " <> hotelName hotel

test :: IO ()
test = runReader sayHelloToGentelmen (Hotel "dfsafs")