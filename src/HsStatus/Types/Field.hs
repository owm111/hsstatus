module HsStatus.Types.Field
  ( Field (..)
  ) where

import Control.Concurrent (ThreadId, MVar)
import Control.Concurrent.STM.TSem (TSem)
import Control.Concurrent.STM.TVar (TVar)

newtype Field a = Field (TSem -> MVar () -> TVar a -> IO [ThreadId])
