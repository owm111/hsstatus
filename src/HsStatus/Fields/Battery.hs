{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  ) where

import qualified Data.ByteString as BS
import System.IO

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.Utils

-- TODO: Int version.
data BattState
  = Discharging Double
  | Charging Double
  | NotCharging Double
  | Full
  | Unknown
  deriving Show

toStateF :: IOString -> Double -> BattState
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
batteryMonitor :: Int -> Int -> FormatterFor BattState -> IO Field
batteryMonitor interval digits format = do
  max <- withFile full ReadMode BS.hGetLine
  nowH <- openFile now ReadMode
  statusH <- openFile status ReadMode
  let getPerc x = readPercentTruncatedTo digits x max
      go = do 
              hSeek nowH AbsoluteSeek 0
              hSeek statusH AbsoluteSeek 0
              stateF <- toStateF <$> BS.hGetLine statusH
              format <$> stateF <$> getPerc <$> BS.hGetLine nowH
  return $ runEvery interval go
  where dir = "/sys/class/power_supply/BAT0/"
        full = dir ++ "charge_full"
        now = dir ++ "charge_now"
        status = dir ++ "status"
