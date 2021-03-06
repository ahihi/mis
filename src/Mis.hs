{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, TypeOperators #-}

module Mis
    ( app
    , runApp
    ) where

import Control.Applicative
import Control.Concurrent.BroadcastChan
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Servant
import Servant.API
import System.Environment
import System.Exit

data GpsInfo = GpsInfo
  { latitude :: Double
  , longitude :: Double
  , accuracy :: Double
  , direction :: Double
  , speed :: Double
  , time :: UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON GpsInfo

data Environment = Environment
  { gpsInfoRef :: IORef GpsInfo
  , channel :: BroadcastChan In GpsInfo
  , writeKey :: Text
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
  :> QueryParam "time" UTCTime
  :> QueryParam "key" Text
  :> Get '[JSON] WriteGpsResult

writeGps :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
         -> Maybe Double -> Maybe UTCTime -> Maybe Text -> MisHandler WriteGpsResult
writeGps (Just lat) (Just long) (Just acc) (Just dir) (Just spd) (Just t) (Just key') = do
  liftIO $ print acc >> print dir >> print spd >> print t
  
  ref <- gpsInfoRef <$> ask
  chan <- channel <$> ask
  key <- writeKey <$> ask

  if key' == key
    then do
      let updateGpsInfo info = (info', info')
            where
              info' = info
                { latitude = lat
                , longitude = long
                , accuracy = acc
                , direction = dir
                , speed = spd
                , time = t
                }
  
      liftIO $ do
        gpsInfo <- atomicModifyIORef' ref updateGpsInfo 
        writeBChan chan gpsInfo 

      return $ WriteGpsResult "ok"
    else return $ WriteGpsResult "wrong key"
writeGps lat long acc dir spd t key' = do
  liftIO $ do
    putStrLn "error"
    print lat
    print long
    print acc
    print dir
    print spd
    print t
    print key'
    
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

listenerApp :: Environment -> Application
listenerApp env = websocketsOr WS.defaultConnectionOptions wsApp backupApp
  where
    wsApp pending = do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn 30
  
      let inChan = channel env
      chan <- newBChanListener inChan

      let ref = gpsInfoRef env
      initialGpsInfo <- readIORef ref

      let sendGpsInfo = WS.sendTextData conn . encode
      sendGpsInfo initialGpsInfo
  
      forever $ do
        gpsInfo <- readBChan chan
        sendGpsInfo gpsInfo

    backupApp _ respond = respond $ responseLBS status400 [] "nyoro~n"

type FullApi = MisApi :<|> "ws" :> Raw :<|> Raw

fullApi :: Proxy FullApi
fullApi = Proxy

app :: Environment -> Application
app env = serve fullApi combinedServer
  where
    combinedServer :: Server FullApi
    combinedServer = server env
                     :<|> Tagged (listenerApp env)
                     :<|> serveDirectoryFileServer "static"

runApp :: IO ()
runApp = do
  port <- parsePort =<< requireEnv "MIS_PORT"
  key <- requireEnv "MIS_WRITE_KEY"
  
  currentTime <- getCurrentTime
  ref <- newIORef $ GpsInfo
    { latitude = 0/0
    , longitude = 0/0
    , accuracy = 0/0
    , direction = 0/0
    , speed = 0/0
    , time = currentTime
    }
  chan <- newBroadcastChan
  
  let env = Environment
            { gpsInfoRef = ref
            , channel = chan
            , writeKey = Text.pack key
            }
            
  run port $ app env
  where
    requireEnv var = do
      valueMay <- lookupEnv var
      case valueMay of
        Just value -> return value
        Nothing -> do
          putStrLn $ "Environment variable not provided: " ++ var
          exitFailure
      
    parsePort str = case (reads :: ReadS Port) str of
      [(port, "")] -> return port
      _ -> do
        putStrLn $ "Invalid port: " ++ str
        exitFailure
      
