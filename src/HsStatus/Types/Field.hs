module HsStatus.Types.Field
  ( Field (..)
  ) where

import Control.Concurrent
import Data.ByteString
import Data.IORef

newtype Field = Field (Int -> Chan (Int, ByteString) -> IO ())
