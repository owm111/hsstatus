module HsStatus.Fields.ReadHandle (readHandle) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Concurrent.STM.TSem (signalTSem)
import Control.Monad (forever, when)
import Data.ByteString (ByteString, hGetLine)
import System.IO (Handle, hIsEOF)
import System.Posix.Signals (raiseSignal, softwareTermination)

import HsStatus.Types.Field (Field (..))

readHandle :: Handle -> Field ByteString
readHandle handle =
  let go printSem var = do
        tid <- forkIO . forever $ do
          finished <- hIsEOF handle
          when finished (raiseSignal softwareTermination)
          line <- hGetLine handle
          atomically (writeTVar var line >> signalTSem printSem)
        return [tid]
   in Field (mempty, go)
