module HsStatus.Types.Field
  ( Field (..)
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TSem (TSem)
import Control.Concurrent.STM.TVar (TVar)

newtype Field a = Field (TSem -> TVar a -> IO [ThreadId])
