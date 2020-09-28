module HsStatus.Types.Field
  ( Field (..)
  ) where

import Control.Concurrent
import Data.ByteString
import Data.IORef

newtype Field a = Field (Int -> MVar () -> Chan (Int, ByteString) -> IO ())
