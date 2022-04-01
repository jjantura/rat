{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as BSL
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics

import           Network.MQTT.Client
import           Network.URI

data Config = Config {
  server :: Server
} deriving (Show, Eq, Generic)

instance A.FromJSON Config

data Server = Server {
   broker     :: Text
 , port       :: Int
 , channelC2R :: Text
 , channelR2C :: Text
} deriving (Show, Eq, Generic)

instance A.FromJSON Server

data Command = PING | KILL deriving (Enum, Show, Eq)

-- read configuration
readConfig :: (MonadIO m, MonadError e m) => m (Either String Config)
readConfig = do
  configBS <- liftIO $ BSL.readFile "config.json" -- TODO: error handling
  let config = A.eitherDecode configBS :: (Either String Config)
  pure config

handleMessage :: Config -> IO ()
handleMessage config = do
  let (Just uri) = parseURI $ T.unpack . broker . server $ config -- TODO: error handling
  client <- connectURI mqttConfig{_msgCB=SimpleCallback msgReceived} uri
  print =<< subscribe client [(channelC2R . server $ config, subOptions)] []
  waitForClient client
  where
    msgReceived _ t m p = print (t,m,p)

reactive :: IO ()
reactive = do
  config <- readConfig
  case config of
    Left err      -> putStrLn $ "Error: " <> err
    Right config' -> handleMessage config'

main :: IO ()
main = do
  -- spawn reactive thread
  reactive
