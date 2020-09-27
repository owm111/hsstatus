module HsStatus.Fields.Date
  ( dateField
  ) where

import Data.Time

import Control.Concurrent
import Control.Monad
import Data.Time
import Data.IORef

import HsStatus.Types.Field (Field (..))

dateField :: Int -> String -> Field String
dateField delay format = Field $ \printSem _ var -> do
  let formatF = formatTime defaultTimeLocale format
  forever $ do
    t <- formatF <$> getCurrentTime
    writeIORef var t
    tryPutMVar printSem ()
    threadDelay delay

{-# INLINE dateField #-}
