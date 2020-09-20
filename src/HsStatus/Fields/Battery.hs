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

andRewind :: (Handle -> IO a) -> Handle -> IO a
andRewind f h = f h <* hRewind h

statusPair :: Char -> IO Int -> IO (BattState, Int)
statusPair 'F' _ = pure (Full, 100)
statusPair 'C' n = (Charging,)    <$> n
statusPair 'D' n = (Discharging,) <$> n
statusPair 'N' n = (NotCharging,) <$> n
statusPair  _  _ = pure (Unknown, 0)

batteryMonitor :: String -> Int -> Field (BattState, Int)
batteryMonitor name delay = Field $ \printSem var -> do
  let status   = "/sys/class/power_supply/" ++ name ++ "/status"
      capacity = "/sys/class/power_supply/" ++ name ++ "/capacity"
  tid <- forkIO $ do
          statusH <- openFile status ReadMode
          capacityH <- openFile capacity ReadMode
          forever $ do
            statusC <- hGetChar `andRewind` statusH
            pair <- statusPair statusC (read <$> hGetLine `andRewind` capacityH)
            atomically (writeTVar var pair >> signalTSem printSem)
            threadDelay delay
  return [tid]
