{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, TypeOperators #-}

module Mis
    ( app
    , runApp
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

data GpsInfo = GpsInfo
  { latitude :: Double
  , longitude :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON GpsInfo

data Environment = Environment
  { gpsInfoRef :: IORef GpsInfo
  }

type MisHandler = ReaderT Environment Handler

data WriteGpsResult = WriteGpsResult { status :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON WriteGpsResult

type ReadGpsApi = "gps"
  :> Get '[JSON] GpsInfo

readGps :: MisHandler GpsInfo
readGps = do
  ref <- gpsInfoRef <$> ask
  info <- liftIO $ readIORef ref

  return info

type WriteGpsApi = "writeGps"
  :> QueryParam "latitude" Double
  :> QueryParam "longitude" Double
  :> Get '[JSON] WriteGpsResult

writeGps :: Maybe Double -> Maybe Double -> MisHandler WriteGpsResult
writeGps (Just lat) (Just long) = do
  liftIO $ print lat
  liftIO $ print long
  
  ref <- gpsInfoRef <$> ask

  let updateGpsInfo info = (info', ())
        where
          info' = info
            { latitude = lat
            , longitude = long
            }
  
  liftIO $ atomicModifyIORef' ref updateGpsInfo 

  return $ WriteGpsResult "ok"
writeGps _ _ = do
  return $ WriteGpsResult "error"

type MisApi = ReadGpsApi :<|> WriteGpsApi

misApi :: Proxy MisApi
misApi = Proxy

server :: Environment -> Server MisApi
server env = enter nat handler
  where
    handler = readGps :<|> writeGps
    
    nat :: MisHandler :~> Handler
    nat = runReaderTNat env

app :: Environment -> Application
app env = serve misApi $ server env

runApp :: IO ()
runApp = do
  ref <- newIORef $ GpsInfo { latitude = 0/0, longitude = 0/0 }
  let env = Environment ref
  run 8081 $ app env
