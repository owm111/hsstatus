{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  ) where

import Control.Concurrent
import Data.Function
import HsStatus.Types.Field
import Streamly
import Streamly.Memory.Array (Array)
import System.IO

import qualified Streamly.Prelude as S

data BattState
  = Discharging
  | Charging
  | NotCharging
  | Full
  | Unknown
  deriving (Eq, Show, Read)

batteryMonitor :: String -> Int -> (BattState -> Int -> Array Char) -> Field
batteryMonitor name delay format =
  S.bracket ((,) <$> openFile status ReadMode <*> openFile capacity ReadMode) (\ (s,c) -> hClose s >> hClose c) $ \(s,c) ->
    S.repeatM (getPair s c <* threadDelay delay)
      & S.map (uncurry format)
  where status   = "/sys/class/power_supply/" ++ name ++ "/status"
        capacity = "/sys/class/power_supply/" ++ name ++ "/capacity"

{-# INLINE batteryMonitor #-}

getPair :: Handle -> Handle -> IO (BattState, Int)
getPair status capacity = do
  s <- getStatus
  case s of
    'F' -> pure (Full, 100)
    'C' -> pair Charging <$> getCapacity
    'D' -> pair Discharging <$> getCapacity
    'N' -> pair NotCharging <$> getCapacity
    _ -> pure unknown
  where getCapacity = hGetLine `andRewind` capacity >>= readIO
        getStatus   = hGetChar `andRewind` status
        pair s      = ((,) s)
        unknown     = (Unknown, 0)

andRewind f h = f h <* hSeek h AbsoluteSeek 0
