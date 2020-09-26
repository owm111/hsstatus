{-# LANGUAGE TupleSections #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  ) where

import Control.Concurrent
import Control.Monad
import System.IO

import HsStatus.Types.Field
import HsStatus.Types.FieldValue

data BattState
  = Discharging
  | Charging
  | NotCharging
  | Full
  | Unknown
  deriving (Eq, Show, Read)

instance FieldValue BattState where initialValue = Unknown

hRewind :: Handle -> IO ()
hRewind h = hSeek h AbsoluteSeek 0

andRewind :: (Handle -> IO a) -> Handle -> IO a
andRewind f h = f h <* hRewind h

statusPair :: Char -> String -> (BattState, Int)
statusPair 'F' _ = (Full, 100)
statusPair 'C' n = (Charging, read n)
statusPair 'D' n = (Discharging, read n)
statusPair 'N' n = (NotCharging, read n)
statusPair  _  _ = (Unknown, 0)

batteryMonitor :: String -> Int -> Field (Handle, Handle) (Char, String) (BattState, Int)
batteryMonitor name delay = Field
  { acquire = do
    let status   = "/sys/class/power_supply/" ++ name ++ "/status"
        capacity = "/sys/class/power_supply/" ++ name ++ "/capacity"
    statusH <- openFile status ReadMode
    capacityH <- openFile capacity ReadMode
    pure (statusH, capacityH)
  , release = \(h, g) -> hClose g >> hClose g
  , collect = \_ (status, capacity) -> do
    threadDelay delay
    statusC <- hGetChar `andRewind` status
    capacityS <- hGetLine `andRewind` capacity
    pure (statusC, capacityS)
  , process = uncurry statusPair
  }
