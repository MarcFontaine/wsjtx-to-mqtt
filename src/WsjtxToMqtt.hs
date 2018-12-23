{-# Language DataKinds, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack,putStrLn)
import qualified Data.ByteString.Char8 as BS (putStrLn)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Options (runSubcommand, subcommand)
import Data.Aeson as Aeson (eitherDecode, encode)
import Data.Yaml as Yaml (encode)
import Paths_wsjtx_to_mqtt (version)
import Data.Version (showVersion)
import Network.Socket (withSocketsDo)

import W2MTypes
import Bridge (runBridge,testMqtt)
import Config (getConfig, helpConfig)
import WSJTX.UDP.Server (testDump, replyWithPackages, withWsjtxSocket, forkWsjtxServer)
import WSJTX.UDP.NetworkMessage (Package(..), Decode(..))

main :: IO ()
main = withSocketsDo $ Options.runSubcommand [
   Options.subcommand "showConfig" showConfig
  ,Options.subcommand "forward" forward
  ,Options.subcommand "dumpWsjtx" dumpWsjtx
  ,Options.subcommand "sendToUdp" sendToUDP
  ,Options.subcommand "testMqtt" $ \_ EmptyOptions _ -> testMqtt
  ]

dumpWsjtx :: MainOptions -> DumpOptions -> [String] -> IO ()
dumpWsjtx _ format _ = case format of
    DumpHaskell -> void $ testDump
    DumpText    -> server dumpText
    DumpJSON    -> server $ BSLC.putStrLn . Aeson.encode
    where
        server :: (Package -> IO ()) -> IO ()
        server dump = withWsjtxSocket wsjtxPort $ \sock -> do
                        _threadId <- forkWsjtxServer sock dump
                        void getLine
          
        wsjtxPort = 2237

        dumpText :: Package -> IO ()
        dumpText (PDecode (Decode {..})) = Text.putStrLn decode_message
        dumpText _ = return ()

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
       Right packageList -> replyWithPackages port packageList
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
