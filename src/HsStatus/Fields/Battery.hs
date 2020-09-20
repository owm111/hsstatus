{-# LANGUAGE TupleSections #-}

module HsStatus.Fields.Battery 
  ( BattState (..)
  , batteryMonitor
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
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

statusPair :: Char -> IO Int -> IO (BattState, Int)
statusPair 'F' _ = pure (Full, 100)
statusPair 'C' n = (Charging,)    <$> n
statusPair 'D' n = (Discharging,) <$> n
statusPair 'N' n = (NotCharging,) <$> n
statusPair  _  _ = pure (Unknown, 0)

batteryMonitor :: String -> Int -> Field (BattState, Int)
batteryMonitor name delay =
  let status   = "/sys/class/power_supply/" ++ name ++ "/status"
      capacity = "/sys/class/power_supply/" ++ name ++ "/capacity"
      f printSem var = do
        statusH <- openFile status ReadMode
        capacityH <- openFile capacity ReadMode
        fmap (:[]) . forkIO . forever $ do
          statusC <- hGetChar statusH
          hRewind statusH
          pair <- statusPair statusC (readIO =<< hGetLine capacityH <* hRewind capacityH)
          atomically (writeTVar var pair >> signalTSem printSem)
          threadDelay delay
   in Field ((Unknown, 0), f)
