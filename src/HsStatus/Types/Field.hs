module HsStatus.Types.Field
  ( Field (..)
  ) where

import Control.Concurrent (ThreadId, MVar)
import Data.IORef

newtype Field a = Field (MVar () -> MVar () -> IORef a -> IO ())
