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
import           Control.Monad
import           Data.ByteString.Lazy.Char8 as BSLC (unpack)
import           Data.Text as Text

import           MQTT
import qualified WSJTX.UDP.NetworkMessage as WSJTX
import           WSJTX.UDP.Server

import           FormatMqttMsg (toMqttMsg)
import           W2MTypes

runBridge :: W2MTypes.Config -> IO ()
runBridge config
    = withWsjtxSocket wsjtxPort
      $ \wsjtxSocket -> withMqttPublish mqttConfig $ \publish -> do
    debugPrint config "starting processes"
    wsjtxState <- newMVar Nothing
    _wsjtxThread <- forkWsjtxServer wsjtxSocket
                      (udpConsumer
                       config
                       publish
                       wsjtxState
                      )
                      
    debugPrint config "starting processes2" 
    case (beacon_interval serverConf) of
        Nothing -> return ()
        Just t -> do
            debugPrint config "starting beacon transmitter"
            forkBeacon t config publish wsjtxState

    _ <- getLine
    return ()
    where
        wsjtxPort = fromInteger $ udp_port $ config_wsjtx config
        serverConf = config_report_broker config
        mqttConfig = MQTT.PublishConfig
               {
                _pcUsername = Nothing
              , _pcPassword = Nothing
              , _pcHost     = "127.0.0.1"
              , _pcPort     = "1883"
              , _pcClientID = "hspub01"
              }

        udpConsumer mainConfig publish wsjtxStatus package = do
            debugPrint config $ "UDP recieved :" ++ show package
            status <- fmap join $ tryReadMVar wsjtxStatus
            case package of
                WSJTX.PDecode msg -> do
                    let (t,m) =  toMqttMsg mainConfig status msg
                    _ <- publish $ Msg t m
                    debugPrint config $ "MQTT send :" ++ BSLC.unpack m

                WSJTX.PStatus s -> modifyMVar_ wsjtxStatus (const $ return $ Just s)
                _  -> return ()

        forkBeacon delay _mainConfig publish _wsjtxStatus
            = void $ forkIO $ forM_ [(1::Int)..] $ \cnt -> do
            let t =  ( head_topics $ config_report_broker config )
                  ++ [ beacon_topic $ config_report_broker config ]
                  ++ [ Text.pack $ show cnt ]
            _ <- publish $ Msg t "ping"
            debugPrint config $ "MQTT send : ping"
            threadDelay $ delay*1000
