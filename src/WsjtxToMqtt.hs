{-# Language DataKinds, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack)
import qualified Data.ByteString.Char8 as BS (putStrLn)
import qualified Options (runSubcommand, subcommand)
import Data.Aeson as Aeson (eitherDecode)
import Data.Yaml (encode)
import Paths_wsjtx_to_mqtt (version)
import Data.Version (showVersion)
import Network.Socket (withSocketsDo)
import W2MTypes
import Bridge (runBridge)
import Config (getConfig, helpConfig)
import WSJTX.UDP.Server (testDump, replyWithPackages)

main :: IO ()
main = withSocketsDo $ Options.runSubcommand [
   Options.subcommand "showConfig" showConfig
  ,Options.subcommand "forward" forward
  ,Options.subcommand "dumpWsjtx" dumpWsjtx
  ,Options.subcommand "sendToUdp" sendToUDP
  ]

dumpWsjtx :: MainOptions -> EmptyOptions -> [String] -> IO ()
dumpWsjtx _ EmptyOptions _ = void $ testDump

showConfig :: MainOptions -> EmptyOptions -> [String] -> IO ()
showConfig cmdOpts EmptyOptions _ = do
  putStrLn $ "# Version : " ++ showVersion version
  helpConfig
  putStrLn "---------------" 
  putStrLn "current config:"
  config <- getConfig cmdOpts
  BS.putStrLn $ encode config
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
