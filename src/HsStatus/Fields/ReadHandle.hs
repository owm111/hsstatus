module HsStatus.Fields.ReadHandle (readHandle) where

import Control.Concurrent (putMVar)
import Control.Monad (forever, when)
import Data.ByteString (ByteString, hGetLine)
import System.IO (Handle, hIsEOF)

import HsStatus.Types.Field (Field (..))

readHandle :: Handle -> Field () ByteString ByteString
readHandle handle = Field
  { acquire = pure ()
  , release = pure
  , collect = \mvar _ -> do
    finished <- hIsEOF handle
    when finished (putMVar mvar ())
    hGetLine handle
  , process = id
  }
