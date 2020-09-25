module HsStatus.Fields.ReadHandle (readHandle) where

import Control.Concurrent (putMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Concurrent.STM.TSem (signalTSem)
import Control.Monad (forever, when)
import Data.ByteString (ByteString, hGetLine)
import System.IO (Handle, hIsEOF)

import HsStatus.Types.Field (Field (..))

readHandle :: Handle -> Field ByteString
readHandle handle = Field $ \printSem mvar var ->
  forever $ do
    finished <- hIsEOF handle
    when finished (putMVar mvar ())
    line <- hGetLine handle
    atomically (writeTVar var line >> signalTSem printSem)
