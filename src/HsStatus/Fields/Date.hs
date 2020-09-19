module HsStatus.Fields.Date
  ( dateField
  ) where

import Data.Time

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad
import Data.Time

import HsStatus.Types.Field (Field (..))

dateField :: Int -> String -> Field String
dateField delay format =
  let formatF = formatTime defaultTimeLocale format
      go printSem var = do
        tid <- forkIO . forever $ do
          t <- formatF <$> getCurrentTime
          atomically (writeTVar var t >> signalTSem printSem)
          threadDelay delay
        return [tid]
   in Field ("", go)
