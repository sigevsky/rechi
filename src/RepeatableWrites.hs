module RepeatableWrites where

import Network.HTTP.Req
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import qualified Data.Text.IO as T (putStrLn) 
import UnliftIO.Async

data Errs = FailedToExtractToken deriving (Typeable, Show)

instance Exception Errs

aquireToken :: T.Text  -> IO T.Text 
aquireToken acc = runReq defaultHttpConfig $ do
    r <- req POST url dt jsonResponse (contentType <> auth)
    let mbToken = parseMaybe (.: "access_token") (responseBody r)
    maybe (liftIO . throwIO $ FailedToExtractToken) return mbToken
  where
    url = https "account-public-service-gamedev.ol.epicgames.net" /: "account" /: "api" /: "oauth" /: "token"
    contentType = header "Content-Type" "application/x-www-form-urlencoded"
    auth = header "Authorization" ("Basic " <> encodeUtf8 acc)
    dt = ReqBodyBs "grant_type=client_credentials"


registerAssets :: T.Text -> IO ()
registerAssets token = do 
  T.putStrLn "Registring assets!"
  runReq defaultHttpConfig $ do
    req PUT url (ReqBodyJson payload) ignoreResponse (contentType <> auth <> port 8082)
    return ()
  T.putStrLn "Done!"
  where
    host = "localhost"
    productId = "6441301ed55a4caf874f722252d1293a"
    url = http host /: "product" /: "lifecycle" /: "api" /: "admin" /: "v1" /: "promotion" /: "products" /: productId
    contentType = header "Content-Type" "application/json"
    auth = header "Authorization" ("Bearer " <> encodeUtf8 token)
    payload =
        object
          [ "artifactIds" .= ["eb296acb8c3248cebd310b7e51da36b6" :: String]
          ]


parallelAssets :: T.Text -> IO ()
parallelAssets token = pooledReplicateConcurrentlyN_ 4 4 (registerAssets token)
