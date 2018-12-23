----------------------------------------------------------------------------
-- |
-- Module      :  W2MTypes
-- Copyright   :  (c) Marc Fontaine 2017-2018
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module W2MTypes where

import           Data.Aeson as Aeson
import qualified Data.ByteString ()
import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Default.Class
import           Data.Fixed
import           Data.Text (Text)
import           Data.Time
import           Data.Word
import           Data.Yaml as Yaml
import           GHC.Generics
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Options hiding (Options, defaultOptions)
import qualified Options

data MainOptions = MainOptions
    { _debug       :: Bool
     ,_configPath  :: Maybe String
     ,_testDefault :: Bool
     }

instance Options.Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "debug" False
            "enable debugging output"
        <*> simpleOption "config" Nothing
            "path of the configfile"
        <*> simpleOption "testDefault" False
            "just for testing ignore config file and use default config"

data DumpOptions
  = DumpText | DumpHaskell | DumpJSON
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Options.Options DumpOptions where
    defineOptions = defineOption (optionType_enum "format" ) (\o -> o
            { optionLongFlags = ["format"]
            , optionDefault = DumpHaskell
            })

data EmptyOptions = EmptyOptions
instance Options.Options EmptyOptions
  where defineOptions = pure EmptyOptions

data Config = Config {
    config_reporter         :: ReporterConfig
  , config_report_broker    :: BrokerConfig
  , config_enable_report    :: Bool
  , config_wsjtx            :: WSJTXConfig
  , config_protocol_version :: Text
  , config_debug            :: Bool
  } deriving (Show, Read, Eq, Generic, Lift)

instance ToJSON Config where
  toJSON = genericToJSON aesonOptionsDropPrefix
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptionsDropPrefix

instance Default Config where
  def = Config {
    config_reporter= def
  , config_report_broker = def
  , config_enable_report = True
  , config_wsjtx = def
  , config_protocol_version ="W2M-V0.1"
  , config_debug = False
  }

data BrokerConfig = BrokerConfig {
   username        :: Maybe Text
  ,password        :: Maybe Text
  ,host            :: String
  ,head_topics     :: [Text]
  ,beacon_interval :: Maybe Int
  ,beacon_topic    :: Text
  } deriving (Show, Read, Eq, Generic, Lift)

instance ToJSON BrokerConfig where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BrokerConfig

instance Default BrokerConfig where
  def = BrokerConfig {
   username = Just "myUserName"
  ,password = Just "secret"
  ,host     = "localhost"
--  ,port     =
  ,head_topics = ["hamradio","spot"]
  ,beacon_interval = Nothing
  ,beacon_topic = "test-beacon"
  }

data ReporterConfig = ReporterConfig {
   reporter_callsign    :: Maybe Text
  ,reporter_locator     :: Text
  ,reporter_info        :: Text
  ,reporter_band_config :: BandReport
  } deriving (Show, Read, Eq, Generic, Lift)
instance ToJSON ReporterConfig where
  toJSON = genericToJSON aesonOptionsDropPrefix
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON ReporterConfig where
  parseJSON = genericParseJSON aesonOptionsDropPrefix

instance Default ReporterConfig where
  def = ReporterConfig {
   reporter_callsign = Just "E8TST"
  ,reporter_locator  = "FE43ab"
  ,reporter_info  = "rig : FT-840,  Antenna : Vertical"
  ,reporter_band_config = TrackWSJTXBand
  }

data BandReport
  = UnknownBand
  | FixedBand Text
  | TrackWSJTXBand
  deriving (Show, Read, Eq, Generic, Lift)

instance ToJSON BandReport where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BandReport

data WSJTXConfig = WSJTXConfig {
   udp_port  :: Integer
  } deriving (Show, Read, Eq, Generic, Lift)

instance ToJSON WSJTXConfig where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON WSJTXConfig

instance Default WSJTXConfig where
  def = WSJTXConfig {
   udp_port = 2237
  }

data Report = Report {
   message         :: Text
  ,mode            :: Text
  ,band            :: Text
  ,freq            :: Word64
  ,time            :: DiffTime
  ,snr             :: Int
  ,delta_time      :: Fixed E2
  ,delta_frequency :: Word32
  ,recv_locator    :: Text
  ,recv_callsign   :: Text
  ,recv_info       :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Report where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Report

quoteConfig :: QuasiQuoter
quoteConfig = QuasiQuoter {
   quoteExp = \str -> do
       a <- lift str
       b <- lift $ stringToConfig str
       return $ TupE [a,b]
  ,quotePat = const $ error "No quotePat defined for quoteConfig"
  ,quoteType = const $ error "No quoteType defined for quoteConfig"
  ,quoteDec = const $ error "No quoteDec defined for quoteConfig"
  }

stringToConfig :: String -> Config
stringToConfig str
  = case Yaml.decodeEither' $ BS.pack str of
     Right x  -> x
     Left msg -> error $ ("stringToConfig " ++ show  msg)

aesonOptionsDropPrefix :: Options
aesonOptionsDropPrefix
  = defaultOptions {
      fieldLabelModifier = tail . dropWhile (not . (==) '_')
    }

debugPrint :: Config -> String -> IO ()
debugPrint c s = if config_debug c
  then putStrLn s
  else return ()
