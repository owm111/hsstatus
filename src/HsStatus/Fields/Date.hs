module HsStatus.Fields.Date
  ( dateField
  ) where

import Data.Time

import HsStatus.FieldUtils (runEvery)
import HsStatus.Types.Field (Field (..))

dateField :: Int -> String -> Field String
dateField delay format = runEvery delay ""
  (formatTime defaultTimeLocale format <$> getCurrentTime)
