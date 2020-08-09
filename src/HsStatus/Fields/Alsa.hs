{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Fields.Alsa
  ( alsaMonitor
  , AlsaState (..)
  , AlsaPaths (..)
  , mixerController
  , defaultMaster
  , withStdbuf
  , withAlsactl
  ) where

import Control.Monad
import Data.Function
import Sound.ALSA.Mixer
import System.Process.Typed

import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Types.Field (Field (..))
import HsStatus.Utils

newtype AlsaPaths = AlsaPaths (String, String, String, String)

mixerController :: String -> String -> AlsaPaths
mixerController mixer controller = AlsaPaths (mixer, controller, "alsactl", "stdbuf")

defaultMaster :: AlsaPaths
defaultMaster = mixerController "default" "Master"

withStdbuf :: String -> AlsaPaths -> AlsaPaths
withStdbuf s (AlsaPaths (m,c,a,_)) = AlsaPaths (m,c,a,s)

withAlsactl :: String -> AlsaPaths -> AlsaPaths
withAlsactl a (AlsaPaths (m,c,_,s)) = AlsaPaths (m,c,a,s)

newtype AlsaState a = AlsaState (Bool, a)

-- | Field that display volume information for a given mixer and controller.
-- Requires alsactl and stdbuf to be in PATH.
alsaMonitorFloating :: Int -> AlsaPaths -> IO (Field (AlsaState Double))
alsaMonitorFloating digits (AlsaPaths (mixer,controller,alsactl,stdbuf)) = return $ watchProcess monitorProc procF
  where monitorProc = proc stdbuf ["-oL", alsactl, "monitor", mixer]
                    & setStdout createPipe
                    & setStdin nullStream
        procF send p = getFormattedState >>= send >> let h = getStdout p in forever (hGetLine h >> getFormattedState >>= send)
        getFormattedState = do
          x <- readMixer mixer controller
          return $ case x of
            (Just sw, _, Just now, Just max) -> Right $ AlsaState (sw, percentTruncatedTo digits now max)
            _ -> Left "Something went wrong getting the volume" 

alsaMonitor :: AlsaPaths -> IO (Field (AlsaState Int))
alsaMonitor (AlsaPaths (mixer,controller,alsactl,stdbuf)) = return $ watchProcess monitorProc procF
  where monitorProc = proc stdbuf ["-oL", alsactl, "monitor", mixer]
                    & setStdout createPipe
                    & setStdin nullStream
        procF send p = getFormattedState >>= send >> let h = getStdout p in forever (hGetLine h >> getFormattedState >>= send)
        getFormattedState = do
          x <- readMixer mixer controller
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
