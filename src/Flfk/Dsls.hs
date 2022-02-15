{-# LANGUAGE RankNTypes, GADTs, DataKinds, PolyKinds, TypeOperators, ConstraintKinds, TypeFamilies, FunctionalDependencies, GeneralisedNewtypeDeriving #-}
module Flfk.Dsls where

import qualified Data.ByteString as BS
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Either
import Control.Exception (throwIO, Exception)

import Data.Functor.Identity
import GHC.TypeLits
import GHC.Exts (Constraint)

data SerdeError = MultMappersFailed BS.ByteString deriving Show

data Address = Address { namespace :: String, tp :: String, idt :: String }
newtype Env = Env { eaddr :: Address }

class Serde a where
    ser :: a -> BS.ByteString 
    des :: BS.ByteString -> Either SerdeError a

instance Serde a where

type family HasTy (l :: [*]) b :: Constraint where
  HasTy '[] _ = TypeError (Text "BOOM!")
  HasTy (a ': as) a = ()
  HasTy (a ': as) b = HasTy as b


newtype FRef (as :: [*]) b = FRef { faddr :: Address }

class (MonadReader Env f, MonadState s f) => Remote f s as | f -> s, f -> as where
    writeToEgress :: Serde o => o -> f ()
    sendMsg :: (Serde a, HasTy as a) => a -> FRef as b -> f ()
    replying :: (Serde a, HasTy cs a, HasTy as d) => FRef cs d -> (FRef as b -> a) -> f ()


data Mappers (as :: [*]) a where
    MZ :: Mappers '[] a
    MS :: Serde d => (d -> a) -> Mappers as a -> Mappers (d ': as) a

tryDes :: Serde a => BS.ByteString -> Mappers as a -> Either SerdeError a
tryDes bs mps = go complete
    where complete = MS id mps
          go :: forall as a. Mappers as a -> Either SerdeError a
          go MZ = Left $ MultMappersFailed bs
          go (MS mp nextMp) = case des bs of 
              Right a -> Right $ mp a
              Left s -> go nextMp


data Invocation = Invocation {
    iTo :: Address,
    argFuncM :: BS.ByteString,
    replyTo :: Maybe Address
}

data FState s = FState {
    fstate :: s,
    invocation :: [Invocation],
    eggress :: [BS.ByteString]
}

newtype FuncExec f = FuncExec { foo :: (BS.ByteString, Address) -> f (FState BS.ByteString) }

data FuncErr = FailedDesInput SerdeError deriving Show
instance Exception FuncErr

newtype FuncM s as b = FuncM { runFuncM :: FuncM_ s as b }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError FuncErr)

newtype FuncM_ s as b = InnFuncM { runFuncM_ :: ReaderT Env (ExceptT FuncErr (StateT (FState s) IO)) b }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError FuncErr, MonadState (FState s)) 

mkFunc :: (MonadIO f, Serde a, Serde b, Serde s)
    => Mappers as a -- ^ Mappers transforming different inputs to @a
    -> s -- ^ Initial state
    -> (a -> FuncM s (a ': as) b) -- ^ Dsl for a function
    -> (Address -> FRef (a ': as) b, FuncExec f) -- ^ Reference to a function along 
    -- with a runtime to process request emitted via constructed reference
mkFunc mpp s0 fm = (ref, body')
    where
        ref = FRef
        s0' = FState s0 [] []
        body' = FuncExec (liftIO . body)
        body (arg, faddr) = do 
                a <- either (throwIO . FailedDesInput) pure $ tryDes arg mpp
                (eiRes, fstate) <- flip runStateT s0' . runExceptT . flip runReaderT (Env faddr) . runFuncM_ . runFuncM $ fm a
                case eiRes of 
                    Right v -> undefined
                    Left e -> throwIO e

instance MonadState s (FuncM s as) where
    get = FuncM (gets fstate)
    put s = FuncM (do st <- get; put (st { fstate = s}))

instance Remote (FuncM s as) s as where
  writeToEgress o = FuncM $ modify' (\s -> s { eggress = ser o : eggress s })
  sendMsg a (FRef toAddr) = FuncM $ modify' (\s -> s { invocation = Invocation toAddr (ser a) Nothing : invocation s })
  replying (FRef toAddr) f = FuncM $ ask >>= \sf@(Env saddr) ->
      modify' (\s -> s {invocation = Invocation toAddr (ser (f (FRef saddr))) (Just saddr) : invocation s})