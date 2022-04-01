{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics


data Config = Config {
  server :: Server
} deriving (Show, Eq, Generic)

instance A.FromJSON Config

data Server = Server {
   broker     :: Text
 , port       :: Int
 , channelIn  :: Text
 , channelOut :: Text
} deriving (Show, Eq, Generic)

instance A.FromJSON Server

main :: IO ()
main = do
  -- read configuration
  config <- BSL.readFile "config.json"
  case A.decode config :: Maybe Config of
    Just config' -> print config'
    Nothing      -> putStrLn "parsing failed"
