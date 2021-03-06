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

import           Control.Monad
import qualified Data.Default.Class as Default
import           Data.Maybe
import           Data.Yaml as Yaml
import           System.Environment (lookupEnv)
import           System.Exit (exitFailure)

import           W2MTypes

getConfig :: MainOptions -> IO W2MTypes.Config
getConfig opts
  = if _testDefault opts
       then return Default.def
       else do
  envPath <- lookupEnv "WSJTX_TO_MQTT_CONFIG_PATH"
  let configFile = fromJust (
        (_configPath opts)
        `mplus` envPath
        `mplus` (Just "wsjtx-to-mqtt.yaml")
        )
  when (_debug opts) $ putStrLn $ "config file path :" ++ configFile
  confData <- Yaml.decodeFileEither configFile
  config <- case confData of
    Right x  -> return $ x {config_debug =config_debug x || _debug opts}
    Left err -> do
      putStrLn "Cannot parse config file"
      putStrLn $ show err
      exitFailure
  return config


helpConfig :: IO ()
helpConfig = do
    putStrLn "# possible config file :"
    putStrLn includedConfigStr

includedConfigStr :: String
includedConfig :: W2MTypes.Config
(includedConfigStr,includedConfig) = [quoteConfig|
# some comment
wsjtx:
  udp_port: 2237
protocol_version: W2M-V0.1
reporter:
  info: ! 'rig : FT-840,  Antenna : Vertical'
  callsign: E8TST
  band_config:
    tag: TrackWSJTXBand
  locator: FE43ab
report_broker:
  username: myUserName
  password: secret

#  username: null
#  password: null

  head_topics:
  - hamradio
  - spot
  host: localhost
# host: broker.hivemq.com
# host: test.mosquitto.org
# host: iot.eclipse.org ?

# one ping every 1000 ms
  beacon_interval: 1000
  beacon_topic: test-beacon
enable_report: true
debug: false
|]
