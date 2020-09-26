module HsStatus.Fields.Brightness
  ( brightnessMonitor
  ) where

import System.IO

import qualified System.Linux.Inotify as Inot

import HsStatus.Types.Field

brightnessMonitor :: String -> Field (Int, Handle, Inot.Inotify) (Int, String) Int
brightnessMonitor name = Field
  { acquire = do
    let brightness = "/sys/class/backlight/" ++ name ++ "/brightness"
        max_brightness = "/sys/class/backlight/" ++ name ++ "/max_brightness"
    max <- read <$> withFile max_brightness ReadMode hGetLine
    briH <- openFile brightness ReadMode
    inot <- Inot.init
    Inot.addWatch inot brightness Inot.in_CLOSE_WRITE
    pure (max, briH, inot)
  , release = \(_, h, i) -> hClose h >> Inot.close i
  , collect = \_ (n, h, i) -> do
    Inot.getEvent i
    bri <- hGetLine h
    hSeek h AbsoluteSeek 0
    pure (n, bri)
  , process = \(max, briStr) -> let bri = read briStr in bri * 100 `div` max
  }
