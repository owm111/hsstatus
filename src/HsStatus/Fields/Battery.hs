{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  , BattPaths (..)
  , sysPowerSupply
  ) where

import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Utils

newtype BattPaths = BattPaths (String, String, String)

sysPowerSupply :: String -> String -> String -> BattPaths
sysPowerSupply name now full =
  let dir = "/sys/class/power_supply/" ++ name ++ "/"
  in BattPaths (dir ++ "status", dir ++ now, dir ++ full)

data BattState n
  = Discharging n
  | Charging n
  | NotCharging n
  | Full
  | Unknown

toStateF :: IOString -> n -> BattState n
toStateF "Charging" = Charging
toStateF "Discharging" = Discharging
toStateF "Not charging" = NotCharging
toStateF "Full" = const Full
toStateF _ = const Unknown

-- | Field that displays status and percent remaining of battery.
--
-- TODO: handle exceptions.
-- TODO: close handles?
batteryMonitorFloating :: BattPaths -> Int -> Int -> IO (Field (BattState Double))
batteryMonitorFloating (BattPaths (status, now, full)) interval digits = do
  max <- withFile full ReadMode hGetLine
  nowH <- openFile now ReadMode
  statusH <- openFile status ReadMode
  let getPerc x = readPercentTruncatedTo digits x max
      go = do 
              hSeek nowH AbsoluteSeek 0
              hSeek statusH AbsoluteSeek 0
              stateF <- toStateF <$> hGetLine statusH
              Right <$> stateF <$> getPerc <$> hGetLine nowH
  return $ runEvery interval go

batteryMonitor :: BattPaths -> Int -> IO (Field (BattState Int))
batteryMonitor (BattPaths (status, now, full)) interval = do
  max <- withFile full ReadMode hGetLine
  nowH <- openFile now ReadMode
  statusH <- openFile status ReadMode
  let getPerc x = ((fst $ fromJust $ readInt x) * 100) `div` maxN
      maxN = fst $ fromJust $ readInt max
      go = do
              hSeek nowH AbsoluteSeek 0
              hSeek statusH AbsoluteSeek 0
              stateF <- toStateF <$> hGetLine statusH
              Right <$> stateF <$> getPerc <$> hGetLine nowH
  return $ runEvery interval go
