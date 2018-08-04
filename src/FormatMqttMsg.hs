{-# LANGUAGE OverloadedStrings #-}
module FormatMqttMsg where

import           Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Fixed
import           Data.Text as Text
import           Data.Word

import           W2MTypes
import qualified WSJTX.UDP.NetworkMessage as WSJTX

toMqttMsg ::
     W2MTypes.Config -> Maybe WSJTX.Status -> WSJTX.Decode
  -> ([Text] , BSL.ByteString)
toMqttMsg config status msg
  = (toTopic config status msg report
    ,Aeson.encode report)
  where
    report = toReport config status msg

toReport ::
  W2MTypes.Config -> Maybe WSJTX.Status -> WSJTX.Decode -> Report
toReport config status msg = report
  where
    maybeBand = fmap WSJTX.status_dial_frequency status >>= freqToBand
    bandInfo = case reporter_band_config $ config_reporter config of
       UnknownBand -> "unknown_band"
       FixedBand b  -> b
       TrackWSJTXBand -> case maybeBand of
         Nothing -> "unknown_band"
         Just b  -> b
    reporter = case reporter_callsign $ config_reporter config of
       Nothing -> "unknown_reporter"
       Just cs -> cs
    report = Report {
        message = WSJTX.decode_message msg
       ,mode = maybe  "unknown_mode" WSJTX.status_mode status
       ,band = bandInfo
       ,freq = maybe 0 WSJTX.status_dial_frequency status
       ,time = WSJTX.decode_time msg
       ,snr  = WSJTX.decode_snr msg
       ,delta_time = MkFixed $ round (WSJTX.decode_delta_time msg *100)
       ,delta_frequency = WSJTX.decode_delta_frequency msg
       ,recv_locator = reporter_locator $ config_reporter config
       ,recv_callsign = reporter
       ,recv_info = reporter_info $ config_reporter config
     }

toTopic ::
       W2MTypes.Config -> Maybe WSJTX.Status -> WSJTX.Decode -> Report
   -> [Text]
toTopic config _status _msg report
   =    ( head_topics $ config_report_broker config )
     ++ [
         band report
        ,mode report
        ,"callsign"
        ,recv_callsign report
        ,Text.take 2 $ recv_locator report
        ,if (Text.take 2 $ message report) == "CQ" then "is_cq" else "no_cq"
        -- when
        ,config_protocol_version config
        ]

freqToBand :: Word64 -> Maybe Text
freqToBand f
  = case Prelude.filter isInBand wsjtxBands of
      [(bnd,_)] -> Just bnd
      _         -> Nothing
  where
    overlap = 20000 -- 20kz overlap
    isInBand (_,(s,e)) = s-overlap <= f && f <= e + overlap
    wsjtxBands = [
       ("160m",(  1810000,   2000000))
      ,("80m", (  3500000,   3800000))
      ,("40m", (  7000000,   7200000))
      ,("30m", ( 10100000,  10150000))
      ,("20m", ( 14000000,  14350000))
      ,("17m", ( 18068000,  18168000))
      ,("15m", ( 21000000,  21450000))
      ,("12m", ( 24890000,  24990000))
      ,("10m", ( 28000000,  29700000))
      ,("6m",  ( 50030000,  51000000))
      ,("4m",  ( 70000000,  70500000))
      ]
