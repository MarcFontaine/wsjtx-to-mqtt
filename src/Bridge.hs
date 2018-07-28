----------------------------------------------------------------------------
-- |
-- Module      :  Bridge
-- Copyright   :  (c) Marc Fontaine 2017-2018
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
{-# LANGUAGE OverloadedStrings #-}
module Bridge where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString.Char8 as BSC (unpack)
import           Data.Text as Text

import           Network.MQTT hiding (defaultConfig)
import qualified Network.MQTT as MQTT
import qualified WSJTX.UDP.NetworkMessage as WSJTX
import           WSJTX.UDP.Server

import           FormatMqttMsg (toMqttMsg)
import           W2MTypes

runBridge :: W2MTypes.Config -> IO ()
runBridge config = withWsjtxSocket wsjtxPort $ \wsjtxSocket -> do
  cmds <- mkCommands
  pubChan <- newTChanIO
  let serverConf = config_report_broker config
      mqttConf = (MQTT.defaultConfig cmds pubChan)
              { cUsername = W2MTypes.username serverConf
              , cPassword = W2MTypes.password serverConf
              , cHost     = host serverConf
              }
  wsjtxState <- newMVar Nothing
  debugPrint config "starting udp server"
  _wsjtxThread <- forkWsjtxServer wsjtxSocket
                    (udpConsumer
                       config
                       mqttConf
                       wsjtxState
                    )
  case (beacon_interval serverConf) of
    Nothing -> return ()
    Just t -> do
      debugPrint config "starting beacon transmitter"
      forkBeacon t config mqttConf wsjtxState

  debugPrint config "starting mqtt client"
  terminated <- MQTT.run mqttConf
  print terminated
  debugPrint config "mqtt client terminated"
  where
    wsjtxPort = fromInteger $ udp_port $ config_wsjtx config

    udpConsumer mainConfig mqttConf wsjtxStatus package = do
      debugPrint config $ "UDP recieved :" ++ show package
      status <- fmap join $ tryReadMVar wsjtxStatus
      case package of
        WSJTX.PDecode msg -> do
           let (t,m) =  toMqttMsg mainConfig status msg
           publish mqttConf NoConfirm False t m
           debugPrint config $ "MQTT send :" ++ BSC.unpack m

        WSJTX.PStatus s -> modifyMVar_ wsjtxStatus (const $ return $ Just s)
        _  -> return ()

    forkBeacon delay _mainConfig mqttConfig _wsjtxStatus = void $ forkIO $ forM_ [(1::Int)..] $ \cnt -> do
      let t = MQTT.fromLevels
                (    (head_topics $ config_report_broker config)
                  ++ [beacon_topic $ config_report_broker config
                     ,Text.pack $ show cnt
                     ]
                )
      publish mqttConfig NoConfirm False t "ping"
      debugPrint config $ "MQTT send : ping"
      threadDelay delay
