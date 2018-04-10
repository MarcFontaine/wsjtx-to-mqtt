{-# Language DataKinds, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack)
import qualified Data.ByteString.Char8 as BS (putStrLn)
import Options hiding (Options, defaultOptions)
import Data.Aeson as Aeson (eitherDecode)
import Data.Yaml (encode)
import Paths_wsjtx_to_mqtt (version)
import Data.Version (showVersion)
import W2MTypes
import Bridge (runBridge)
import Config (getConfig, helpConfig)
import WSJTX.UDP.Server (testDump, replyWithPackages)

main :: IO ()
main = runSubcommand [
   subcommand "showConfig" showConfig
  ,subcommand "forward" forward
  ,subcommand "dumpWsjtx" dumpWsjtx
  ,subcommand "sendToUdp" sendToUDP
  ]

dumpWsjtx :: MainOptions -> MainOptions -> [String] -> IO ()
dumpWsjtx _ _ _ = void $ testDump

showConfig :: MainOptions -> MainOptions -> [String] -> IO ()
showConfig cmdOpts _ _ = do
  putStrLn $ "Version : " ++ showVersion version
  helpConfig
  putStrLn "---------------" 
  putStrLn "current config:"
  config <- getConfig cmdOpts
  BS.putStrLn $ encode config
  exitSuccess

sendToUDP :: MainOptions -> MainOptions -> [String] -> IO ()
sendToUDP cmdOpts _ strs = do
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

forward :: MainOptions -> MainOptions -> [String] -> IO ()
forward cmdOpts _opts _args = do
  config <- getConfig cmdOpts
  runBridge config
