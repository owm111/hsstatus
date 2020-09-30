module HsStatus.Fields.Brightness
  ( brightnessMonitor
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import HsStatus.Types.Field
import Streamly
import Streamly.Memory.Array (Array)
import System.IO

import qualified Streamly.Prelude as S
import qualified System.Linux.Inotify as Inot

brightnessMonitor :: String -> (Int -> Array Char) -> Field
brightnessMonitor name format = do
  max <- liftIO $ withFile max_brightness ReadMode (hGetLine >=> readIO)
  let toPerc bri = bri * 100 `div` max
  S.bracket ((,) <$> openFile brightness ReadMode <*> Inot.init) (\ (h,i) -> hClose h >> Inot.close i) $ \ (h,i) -> do
    liftIO $ Inot.addWatch i brightness Inot.in_CLOSE_WRITE
    S.repeatM (Inot.getEvent i *> hGetLine h <* hSeek h AbsoluteSeek 0 >>= readIO)
      & S.map toPerc
      & S.map format
  where max_brightness = "/sys/class/backlight/" ++ name ++ "/max_brightness"
        brightness = "/sys/class/backlight/" ++ name ++ "/brightness"

{-# INLINE brightnessMonitor #-}
