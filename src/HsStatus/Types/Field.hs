module HsStatus.Types.Field
  ( Field
  ) where

import Streamly
import Streamly.Memory.Array (Array)

type Field = Serial (Array Char)
