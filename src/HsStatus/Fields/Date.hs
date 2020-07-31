module HsStatus.Fields.Date
  ( dateField
  , dateFieldInZone
  ) where

import Data.ByteString.Char8 (pack)
import Data.Time

import HsStatus.Types
import HsStatus.FieldUtils

getFormattedDateInZone :: TimeZone -> String -> IO IOString
getFormattedDateInZone zone fmt =
  pack <$> formatTime defaultTimeLocale fmt <$> utcToZonedTime zone <$> getCurrentTime

-- | 'dateFieldInZone' for the current timezone.
dateField :: Int -> String -> IO Field
dateField delay format = do
  zone <- getCurrentTime >>= getTimeZone
  dateFieldInZone zone delay format

-- | Field that displays the current time within a given timezone.
dateFieldInZone :: TimeZone -> Int -> String -> IO Field
dateFieldInZone zone delay = return . runEvery delay . getFormattedDateInZone zone
