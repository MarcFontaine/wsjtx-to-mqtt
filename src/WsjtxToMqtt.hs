{-# Language DataKinds, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Paths_wsjtx_to_mqtt (version)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack,putStrLn)
import qualified Data.ByteString.Char8 as BS (putStrLn, unpack)
import qualified Data.Text as Text (unpack)
import qualified Options (runSubcommand, subcommand)
import Data.Aeson as Aeson (eitherDecode, encode)
import Data.Yaml as Yaml (encode)
import Data.Version (showVersion)
import Data.Double.Conversion.ByteString (toFixed)
import Network.Socket (withSocketsDo)

import W2MTypes
import Bridge (runBridge,testMqtt)
import Config (getConfig, helpConfig)
import WSJTX.UDP.Server (testDump, replyWithPackets, withWsjtxSocket, forkWsjtxServer)
import WSJTX.UDP.NetworkMessage (Packet(..), Decode(..))

main :: IO ()
main = withSocketsDo $ Options.runSubcommand [
   Options.subcommand "showConfig" showConfig
  ,Options.subcommand "forward" forward
  ,Options.subcommand "dumpWsjtx" dumpWsjtx
  ,Options.subcommand "sendToWsjtx" sendToUDP
  ,Options.subcommand "testMqtt" $ \_ EmptyOptions _ -> testMqtt
  ]

dumpWsjtx :: MainOptions -> DumpOptions -> [String] -> IO ()
dumpWsjtx _ format _ = case format of
    DumpHaskell -> void $ testDump
    DumpText    -> server dumpText
    DumpJSON    -> server $ BSLC.putStrLn . Aeson.encode
    where
        server :: (Packet -> IO ()) -> IO ()
        server dump = withWsjtxSocket wsjtxPort $ \sock -> do
                        _threadId <- forkWsjtxServer sock dump
                        void getLine
          
        wsjtxPort = 2237

        dumpText :: Packet -> IO ()
        dumpText (PDecode (Decode {..})) = putStrLn $ concat [
            show decode_time
          , pad 4 $ BS.unpack $ toFixed 1 decode_delta_time
          , pad 5 $ show decode_snr
          , pad 5 $ show decode_delta_frequency
          , " ~  "
          , Text.unpack decode_message
          ]
        dumpText _ = return ()
        pad l s = reverse $ take l $ reverse $ "    " ++ s

showConfig :: MainOptions -> EmptyOptions -> [String] -> IO ()
showConfig cmdOpts EmptyOptions _ = do
  putStrLn $ "# Version : " ++ showVersion version
  helpConfig
  putStrLn "---------------" 
  putStrLn "current config:"
  config <- getConfig cmdOpts
  BS.putStrLn $ Yaml.encode config
  exitSuccess

sendToUDP :: MainOptions -> EmptyOptions -> [String] -> IO ()
sendToUDP cmdOpts EmptyOptions strs = do
  config <- getConfig cmdOpts
  let port = fromInteger $ udp_port $ config_wsjtx config
  let decoded = forM strs (Aeson.eitherDecode . BSLC.pack)
  case decoded of
       Right packageList -> replyWithPackets port packageList
       Left msg -> do
         putStrLn "error : cannot decode packages"
         putStrLn $ show (msg , strs)
         exitFailure
  exitSuccess

forward :: MainOptions -> EmptyOptions -> [String] -> IO ()
forward cmdOpts EmptyOptions _args = do
  config <- getConfig cmdOpts
  debugPrint config "starting wsjtx to udp bridge"
  runBridge config
