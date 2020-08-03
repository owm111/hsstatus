{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  ) where

import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Utils

data BattState n
  = Discharging n
  | Charging n
  | NotCharging n
  | Full
  | Unknown
  deriving Show

toStateF :: IOString -> n -> BattState n
toStateF "Charging" = Charging
toStateF "Discharging" = Discharging
toStateF "Not charging" = NotCharging
toStateF "Full" = const Full
toStateF _ = const Unknown

-- | Field that displays status and percent remaining of battery.
--
-- TODO: pass paths as parameters.
-- TODO: handle exceptions.
-- TODO: close handles?
batteryMonitorFloating :: Int -> Int -> FormatterFor (BattState Double) -> IO Field
batteryMonitorFloating interval digits format = do
  max <- withFile full ReadMode hGetLine
  nowH <- openFile now ReadMode
  statusH <- openFile status ReadMode
  let getPerc x = readPercentTruncatedTo digits x max
      go = do 
              hSeek nowH AbsoluteSeek 0
              hSeek statusH AbsoluteSeek 0
              stateF <- toStateF <$> hGetLine statusH
              format <$> stateF <$> getPerc <$> hGetLine nowH
  return $ runEvery interval go
  where dir = "/sys/class/power_supply/BAT0/"
        full = dir ++ "charge_full"
        now = dir ++ "charge_now"
        status = dir ++ "status"

batteryMonitor :: Int -> FormatterFor (BattState Int) -> IO Field
batteryMonitor interval format = do
  max <- withFile full ReadMode hGetLine
  nowH <- openFile now ReadMode
  statusH <- openFile status ReadMode
  let getPerc x = ((fst $ fromJust $ readInt x) * 100) `div` maxN
      maxN = fst $ fromJust $ readInt max
      go = do
              hSeek nowH AbsoluteSeek 0
              hSeek statusH AbsoluteSeek 0
              stateF <- toStateF <$> hGetLine statusH
              format <$> stateF <$> getPerc <$> hGetLine nowH
  return $ runEvery interval go
  where dir = "/sys/class/power_supply/BAT0/"
        full = dir ++ "charge_full"
        now = dir ++ "charge_now"
        status = dir ++ "status"
