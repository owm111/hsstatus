{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  ) where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import System.IO

import qualified Data.ByteString.Char8 as BS

import HsStatus.Types.Field

data BattState
  = Discharging
  | Charging
  | NotCharging
  | Full
  | Unknown
  deriving (Eq, Show, Read)

hRewind :: Handle -> IO ()
hRewind h = hSeek h AbsoluteSeek 0

andRewind :: (Handle -> IO a) -> Handle -> IO a
andRewind f h = f h <* hRewind h

getPair :: Handle -> Handle -> IO (BattState, Int)
getPair status capacity = do
  s <- getStatus
  case s of
    'F' -> pure (Full, 100)
    'C' -> pair Charging <$> getCapacity
    'D' -> pair Discharging <$> getCapacity
    'N' -> pair NotCharging <$> getCapacity
    _ -> pure unknown
  where getCapacity = BS.hGetLine `andRewind` capacity
        getStatus   = hGetChar `andRewind` status
        pair s      = maybe unknown ((s,) . fst) . BS.readInt
        unknown     = (Unknown, 0)

batteryMonitor :: String -> Int -> (BattState -> Int -> ByteString) -> Field
batteryMonitor name delay format = Field $ \idx chan -> do
  let status   = "/sys/class/power_supply/" ++ name ++ "/status"
      capacity = "/sys/class/power_supply/" ++ name ++ "/capacity"
  withFile status ReadMode $ \statusH -> do
    hSetBuffering statusH NoBuffering
    withBinaryFile capacity ReadMode $ \capacityH ->
      forever (getPair statusH capacityH >>= writeChan chan . (,) idx . uncurry format >> threadDelay delay)

{-# INLINE batteryMonitor #-}
