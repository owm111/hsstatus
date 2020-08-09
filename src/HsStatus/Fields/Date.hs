module HsStatus.Fields.Date
  ( dateField
  , dateFieldInZone
  ) where

import Data.ByteString.Char8 (pack)
import Data.Time

import HsStatus.FieldUtils
import HsStatus.Types.Field (Field (..))

getDateInZone :: TimeZone -> IO ZonedTime
getDateInZone zone = utcToZonedTime zone <$> getCurrentTime

-- | 'dateFieldInZone' for the current timezone.
dateField :: Int -> IO (Field ZonedTime)
dateField delay = do
  zone <- getCurrentTime >>= getTimeZone
  dateFieldInZone zone delay

-- | Field that displays the current time within a given timezone.
dateFieldInZone :: TimeZone -> Int -> IO (Field ZonedTime)
dateFieldInZone zone delay = return $ runEvery delay $ Right <$> getDateInZone zone
