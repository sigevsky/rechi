{-# Language DataKinds #-}
module Flfk.Example () where


import Flfk.Dsls
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State
import qualified Data.Map as MP

(printerRef, printerBody) = mkFunc @IO @String MZ () (liftIO . print)

newtype GreeterReq = GreeterReq { name :: String }
newtype GreeterResp = GreeterResp { n :: Int }

greeter (GreeterReq name) = do
    ni <- state go
    liftIO . print $ "Seen " <> name <> " times " <> show ni
    where go mp = let 
                ps = MP.lookup name mp
                nv = maybe 0 (+1) ps
                nm = maybe (MP.insert name 0 mp) (const $ MP.update (Just . (+1)) name mp) ps
            in (nv, nm)

(greeterRef, greeterBody) = mkFunc @IO MZ MP.empty greeter
