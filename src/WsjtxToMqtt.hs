{-# Language DataKinds, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack)
import Options hiding (Options, defaultOptions)
import Data.Aeson as Aeson (eitherDecode)

import W2MTypes
import Bridge (runBridge)
import Config (getConfig, helpConfig)
import WSJTX.UDP.Server (testDump, replyWithPackages)

main :: IO ()
main = runSubcommand [
   subcommand "help" help
  ,subcommand "forward" forward
  ,subcommand "dumpWsjtx" dumpWsjtx
  ,subcommand "sendToUdp" sendToUDP
  ]

dumpWsjtx :: MainOptions -> MainOptions -> [String] -> IO ()
dumpWsjtx _ _ _ = void $ testDump

help :: MainOptions -> MainOptions -> [String] -> IO ()
help _ _ _ = do
  helpConfig
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
