module HsStatus.Fields.ReadHandle (readHandle) where

import Control.Concurrent (putMVar, forkIO, tryPutMVar)
import Control.Monad (forever, when)
import Data.ByteString (ByteString, hGetLine)
import Data.IORef
import System.IO (Handle, hIsEOF)

import HsStatus.Types.Field (Field (..))

readHandle :: Handle -> Field ByteString
readHandle handle = Field $ \printSem mvar var ->
  forever $ do
    finished <- hIsEOF handle
    when finished (putMVar mvar ())
    line <- hGetLine handle
    writeIORef var line
    tryPutMVar printSem ()

{-# INLINE readHandle #-}
