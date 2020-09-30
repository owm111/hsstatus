module HsStatus.Fields.ReadHandle (readHandle) where

import Data.Function
import HsStatus.Types.Field (Field (..))
import Streamly
import Streamly.Data.Unicode.Stream
import System.IO

import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A

readHandle :: Handle -> Field
readHandle handle =
  S.unfold FH.read handle
    & decodeUtf8
    & S.splitOn (== '\n') A.write

{-# INLINE readHandle #-}
