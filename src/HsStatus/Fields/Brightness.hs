module HsStatus.Fields.Brightness
  ( brightnessMonitor
  ) where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.ByteString.Char8 (ByteString, readInt)
import System.IO

import qualified Data.ByteString.Char8 as BS
import qualified System.Linux.Inotify as Inot

import HsStatus.Types.Field

brightnessMonitor :: String -> (Int -> ByteString) -> Field
brightnessMonitor name format = Field $ \idx chan -> do
  let brightness = "/sys/class/backlight/" ++ name ++ "/brightness"
      max_brightness = "/sys/class/backlight/" ++ name ++ "/max_brightness"
  max <- readIntOr0 <$> withFile max_brightness ReadMode BS.hGetLine
  briH <- openFile brightness ReadMode
  let toPerc = (\bri -> bri * 100 `div` max) . readIntOr0
      update = do
        bri <- BS.hGetLine briH
        hSeek briH AbsoluteSeek 0
        writeChan chan (idx, format $ toPerc bri)
  bracket Inot.init Inot.close $ \inot -> do
    wd <- Inot.addWatch inot brightness Inot.in_CLOSE_WRITE
    forever (update >> Inot.getEvent inot)

{-# INLINE brightnessMonitor #-}

readIntOr0 :: ByteString -> Int
readIntOr0 = maybe 0 fst . readInt
