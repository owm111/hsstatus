{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  , BattPaths (..)
  , sysPowerSupply
  ) where

import Control.Exception
import Control.Monad
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, readInt)
import Data.Functor ((<&>))
import System.IO.Error

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

makeState :: IO (Either IOError ByteString) -> IO (Either IOError a) -> IO (Either IOError (BattState a))
makeState = liftM2 toStateEither
  where toStateEither = liftM2 toStateF

        toStateF "Charging" = Charging
        toStateF "Discharging" = Discharging
        toStateF "Not charging" = NotCharging
        toStateF "Full" = const Full
        toStateF _ = const Unknown

-- | Field that displays status and percent remaining of battery.
--
-- TODO: close handles?
batteryMonitorFloating :: BattPaths -> Int -> Int -> IO (Field (BattState Double))
batteryMonitorFloating (BattPaths (status, now, full)) interval digits = do
  fullN   <- tryIOError (withFile full ReadMode hGetLine) <&> readIntEither
  statusH <- tryIOError (openFile status ReadMode)
  nowH    <- tryIOError (openFile now ReadMode)

  let makePercent :: Either IOError Int -> Either IOError Double
      makePercent = liftM (/ (10^digits)) . liftM fromIntegral . liftM2 div fullN . liftM (* (10^(digits + 2)))

      getStatus :: IO (Either IOError ByteString)
      getStatus = hGetFirstLine statusH

      getPercent :: IO (Either IOError Double)
      getPercent = hGetFirstLine nowH <&> readIntEither <&> makePercent

      go :: IO (Either ByteString (BattState Double))
      go = makeState getStatus getPercent <&> packExceptions

  return (runEvery interval go)

batteryMonitor :: BattPaths -> Int -> IO (Field (BattState Int))
batteryMonitor (BattPaths (status, now, full)) interval = do
  fullN   <- tryIOError (withFile full ReadMode hGetLine) <&> readIntEither
  statusH <- tryIOError (openFile status ReadMode)
  nowH    <- tryIOError (openFile now ReadMode)

  let makePercent :: Either IOError Int -> Either IOError Int
      makePercent = liftM2 div fullN . liftM (*100)

      getStatus :: IO (Either IOError ByteString)
      getStatus = hGetFirstLine statusH

      getPercent :: IO (Either IOError Int)
      getPercent = hGetFirstLine nowH <&> readIntEither <&> makePercent

      go :: IO (Either ByteString (BattState Int))
      go = makeState getStatus getPercent <&> packExceptions

  return (runEvery interval go)
