module HsStatus.Fields.Brightness
  ( brightnessMonitor
  ) where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.IORef
import System.IO

import qualified System.Linux.Inotify as Inot

import HsStatus.Types.Field

brightnessMonitor :: String -> Field Int
brightnessMonitor name = Field $ \printSem _ var -> do
  let brightness = "/sys/class/backlight/" ++ name ++ "/brightness"
      max_brightness = "/sys/class/backlight/" ++ name ++ "/max_brightness"
  tid <- forkIO $ do
    max <- read <$> withFile max_brightness ReadMode hGetLine
    briH <- openFile brightness ReadMode
    let toPerc = (\bri -> bri * 100 `div` max) . read
        update = do
          bri <- hGetLine briH
          hSeek briH AbsoluteSeek 0
          writeIORef var (toPerc bri)
          void (tryPutMVar printSem ())
    bracket Inot.init Inot.close $ \inot -> do
      wd <- Inot.addWatch inot brightness Inot.in_CLOSE_WRITE
      forever (update >> Inot.getEvent inot)
  return [tid]
