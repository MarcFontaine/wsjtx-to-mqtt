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

module Bridge where

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad

import Network.MQTT hiding (defaultConfig)
import qualified Network.MQTT as MQTT
import WSJTX.UDP.Server
import qualified WSJTX.UDP.NetworkMessage as WSJTX

import W2MTypes
import FormatMqttMsg
  
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
  _wsjtxTread <- forkWsjtxServer wsjtxSocket 
                    (udpConsumer
                       config
                       mqttConf
                       wsjtxState
                    )

  terminated <- MQTT.run mqttConf
  print terminated
  where
    wsjtxPort = fromInteger $ udp_port $ config_wsjtx config

    udpConsumer mainConfig mqttConf wsjtxStatus package = do
      status <- fmap join $ tryReadMVar wsjtxStatus
      case package of
        WSJTX.PDecode msg -> do
           let (t,m) =  toMqttMsg mainConfig status msg
           publish mqttConf NoConfirm False t m


        WSJTX.PStatus s -> modifyMVar_ wsjtxStatus (const $ return $ Just s)
        _  -> return ()
