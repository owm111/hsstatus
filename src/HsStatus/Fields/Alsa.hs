{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Fields.Alsa
  ( alsaMonitor
  , AlsaState (..)
  ) where

import Control.Monad
import Data.Function
import Sound.ALSA.Mixer
import System.Process.Typed

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Utils

newtype AlsaState a = AlsaState (Bool, a)

-- | Field that display volume information for a given mixer and controller.
-- Requires alsactl and stdbuf to be in PATH.
--
-- TODO: pass mixer and controller.
alsaMonitorFloating :: Int -> String -> IO (Field (AlsaState Double))
alsaMonitorFloating digits alsactl = return $ watchProcess monitorProc procF
  where monitorProc = proc "stdbuf" ["-oL", "alsactl", "monitor", "default"]
                    & setStdout createPipe
                    & setStdin nullStream
        procF send p = getFormattedState >>= send >> let h = getStdout p in forever (hGetLine h >> getFormattedState >>= send)
        getFormattedState = do
          x <- readMixer "default" "Master"
          return $ case x of
            (Just sw, _, Just now, Just max) -> Right $ AlsaState (sw, percentTruncatedTo digits now max)
            _ -> Left "Something went wrong getting the volume" 

alsaMonitor :: String -> IO (Field (AlsaState Int))
alsaMonitor alsactl = return $ watchProcess monitorProc procF
  where monitorProc = proc "stdbuf" ["-oL", "alsactl", "monitor", "default"]
                    & setStdout createPipe
                    & setStdin nullStream
        procF send p = getFormattedState >>= send >> let h = getStdout p in forever (hGetLine h >> getFormattedState >>= send)
        getFormattedState = do
          x <- readMixer "default" "Master"
          return $ case x of
            (Just sw, _, Just now, Just max) -> Right $ AlsaState (sw, (now * 100) `div` max)
            _ -> Left "Something went wrong getting the volume"

readMixer :: String -> String -> IO (Maybe Bool, Maybe Int, Maybe Int, Maybe Int)
readMixer m c = withMixer m $ \mixer -> do
  control <- getControlByName mixer c
  -- Left <$> works inside IO, right works inside Maybe.
  -- getRange :: Volume -> Maybe (CInt, CInt)
  (min, max) <- maybeGetRange $ volumeOf control
  -- getChannel :: Channel -> PerChannel x -> IO (Maybe x)
  val <- maybeGetFrontLeft (value <$> volumeOf control)
  sw <- maybeGetFrontLeft (switchOf control)
  return (sw, fromEnum <$> min, fromEnum <$> val, fromEnum <$> max)

maybeGetRange :: Maybe Volume -> IO (Maybe CLong, Maybe CLong)
maybeGetRange = maybe (return (Nothing, Nothing)) (fmap (\(x,y) -> (Just x, Just y)) . getRange)

maybeGetFrontLeft :: Maybe (PerChannel x) -> IO (Maybe x)
maybeGetFrontLeft = maybe (return Nothing) (getChannel FrontLeft)

volumeOf :: Maybe Control -> Maybe Volume
volumeOf c = go playback `mplus` go capture `mplus` go common
  where go x = x . volume =<< c
  
switchOf :: Maybe Control -> Maybe Switch
switchOf c = go playback `mplus` go capture `mplus` go common
  where go x = x . switch =<< c
