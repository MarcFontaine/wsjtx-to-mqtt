{-# LANGUAGE OverloadedStrings #-}
module MQTT
where

import Data.Text as Text (Text, intercalate)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL

import Control.Exception.Base (bracket)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forM_, forever, void)
import Network.Socket hiding (send, recv, recvFrom)
import Network.Socket.ByteString.Lazy (sendAll, recv)

data PublishConfig = PublishConfig
  {
     _pcUsername :: Maybe String
   , _pcPassword :: Maybe String
   , _pcHost     :: String
   , _pcPort     :: String
   , _pcClientID :: String
  } deriving Show

defaultPublishConfig :: PublishConfig
defaultPublishConfig = PublishConfig
  {
     _pcUsername = Nothing
   , _pcPassword = Nothing
   , _pcHost     = "127.0.0.1"
   , _pcPort     = "1883"
   , _pcClientID = "hspub01"
  }

data Msg = Msg [Text] !BSL.ByteString
  deriving Show

withMqttPublish :: PublishConfig -> ((Msg -> IO ()) -> IO ()) -> IO ()
withMqttPublish config callback = withSocketsDo $ do
    addr <- resolve (_pcHost config) (_pcPort config)
    channel <- newChan
    void $ forkIO $ bracket (open addr) close $ \sock -> do
        sendAll sock myConnectMsg
        forever $ do
          readChan channel >>= return . publishMsg >>= sendAll sock 
          -- TODO : read any data from the socket
    callback (writeChan channel)
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    myConnectMsg = connectMsg (BSC.pack $ _pcClientID config)

testPublish :: IO ()
testPublish = withMqttPublish defaultPublishConfig $ \publish -> do
    publish $ Msg ["hamradio","topic"] "msg1"
    publish $ Msg ["hamradio","topic"] "msg2"
    publish $ Msg ["hamradio","topic3"] "msg3"
    threadDelay 5000000    

sendMqtt :: [BSL.ByteString] -> IO ()    
sendMqtt packets = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "1883"
    bracket (open addr) close talk
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = do
        forM_ packets $ sendAll sock
        _msg <- recv sock 1024
        Prelude.putStr "Received: "
--        BSC.putStrLn msg
        threadDelay 5000000
                  
connectMsg :: BSC.ByteString -> BSL.ByteString
connectMsg clientId = toLazyByteString $
       connectHeader
    `mappend` remainingLength (
           protocolName
        `mappend` protocolLevel
        `mappend` connectFlags
        `mappend` keepAlive
        `mappend` lenBS clientId
        )  
    where
        connectHeader = word8 0x10      
        protocolName  = lenBS "MQIsdp"
        protocolLevel = word8 0x03
        connectFlags  = word8 0x02
        keepAlive     = word16BE 0

publishMsg :: Msg -> BSL.ByteString
publishMsg (Msg topic message) = toLazyByteString $
    ( pubHeader
      `mappend` remainingLength (
           (lenBS $ encodeUtf8 $ Text.intercalate "/" topic)
        `mappend` identifier
        `mappend` lazyByteString message ))
    where
        pubHeader = word8 0x30
        identifier = mempty 

lenBS :: BSC.ByteString -> Builder
lenBS bs
    = ( word16BE $ fromIntegral $ BSC.length bs) `mappend` byteString bs
    
remainingLength :: Builder -> Builder
remainingLength c =
     word8 len -- variable length
  `mappend` lazyByteString body
  where
    body = toLazyByteString c
    len = fromIntegral $ BSL.length body
