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
  , accuracy :: Double
  , direction :: Double
  , speed :: Double
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
  :> QueryParam "accuracy" Double
  :> QueryParam "direction" Double
  :> QueryParam "speed" Double
  :> QueryParam "time" String
  :> Get '[JSON] WriteGpsResult

writeGps :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe String -> MisHandler WriteGpsResult
writeGps (Just lat) (Just long) (Just acc) (Just dir) (Just spd) (Just t) = do
  liftIO $ print acc >> print dir >> print spd >> print t
  
  ref <- gpsInfoRef <$> ask

  let updateGpsInfo info = (info', ())
        where
          info' = info
            { latitude = lat
            , longitude = long
            , accuracy = acc
            , direction = dir
            , speed = spd
            }
  
  liftIO $ atomicModifyIORef' ref updateGpsInfo 

  return $ WriteGpsResult "ok"
writeGps lat long acc dir spd t = do
  liftIO $ do
    putStrLn "error"
    print lat
    print long
    print acc
    print dir
    print spd
    print t
    
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
  ref <- newIORef $ GpsInfo
    { latitude = 0/0
    , longitude = 0/0
    , accuracy = 0/0
    , direction = 0/0
    , speed = 0/0
    }
  let env = Environment ref
  run 8081 $ app env
