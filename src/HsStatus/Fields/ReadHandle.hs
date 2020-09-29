module HsStatus.Fields.ReadHandle (readHandle) where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString, hGetLine)
import Data.IORef
import System.IO (Handle, hIsEOF)

import HsStatus.Types.Field (Field (..))

readHandle :: Handle -> Field
readHandle handle = Field $ \idx chan ->
  let go = do
        finished <- hIsEOF handle
        unless finished $ do
          line <- hGetLine handle
          writeChan chan (idx, line)
          go
   in go

{-# INLINE readHandle #-}
