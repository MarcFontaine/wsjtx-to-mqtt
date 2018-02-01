----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Copyright   :  (c) Marc Fontaine 2017-2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

{-# LANGUAGE QuasiQuotes #-}
module Config where

import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import Control.Monad
import Data.Maybe
import Text.Jasmine as Jasmine
import Data.Aeson as Aeson
import qualified Data.Default.Class as Default

import W2MTypes

getConfig :: MainOptions -> IO W2MTypes.Config
getConfig opts
  = if _testDefault opts
       then return Default.def
       else do
  envPath <- lookupEnv "WSJTX_TO_MQTT_CONFIG_PATH"
  let configFile = fromJust (
        (_configPath opts)
        `mplus` envPath
        `mplus` (Just "wsmqtt.conf")
        )
  when (_debug opts) $ putStrLn $ "config file path :" ++ configFile
  confData <- Jasmine.minifyFile  configFile
  config <- case eitherDecode confData of
    Right x  -> return x
    Left msg -> do 
      putStrLn "Cannot parse config file"
      putStrLn msg
      exitFailure
  return config


helpConfig :: IO ()
helpConfig = do
    putStrLn "possible config file :" 
    putStrLn includedConfigStr
    
includedConfigStr :: String
includedConfig :: W2MTypes.Config
(includedConfigStr,includedConfig) = [quoteConfig|
// wsjtx-to-mqtt uses json syntax for config files
// comments are allowed
{
    "reporter": {
        "callsign": "E8TST",
        "locator": "FE43ab", 
        "info": "rig : FT-840,  Antenna = Vertical",// add what you like 
        "band_config": {
            "tag": "TrackWSJTXBand"
        }
    },
    "report_broker": {
        "username": "myUserName",
        "password": "secret",
        "head_topics": [
            "hamradio",
            "spot"
        ],
        "host": "localhost"
    },
    "enable_report": true,
    "protocol_version": "W2M-V0.1",   // don't change this
    "wsjtx": {
        "udp_port": 2237           // default udp-port  
    }
}
|]
