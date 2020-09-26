module HsStatus.Fields.Date
  ( dateField
  ) where

import Data.Time

import Control.Concurrent
import Control.Monad
import Data.Time

import HsStatus.Types.Field (Field (..))

dateField :: Int -> String -> Field () UTCTime String
dateField delay format = Field
  { acquire = pure ()
  , release = pure
  , collect = \_ _ -> threadDelay 1000000 >> getCurrentTime
  , process = formatTime defaultTimeLocale format
  }
