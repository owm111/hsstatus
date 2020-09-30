{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Main
  ( hRunHsStatus
  ) where

import Data.Foldable
import Data.Function
import Data.Vector (Vector)
import HsStatus.Types.Field
import Streamly
import Streamly.Data.Unicode.Stream
import System.IO

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S

hRunHsStatus :: [Field] -> IO ()
hRunHsStatus fields = fields
  & zipWith (\ ix st -> S.map ((,) ix) st) [0..]
  & foldWith async
  & S.scanl' setValue (initialStateFor fields)
  & S.map fold
 -- & S.concatUnfold A.read
 -- & encodeUtf8
  & S.map (\a -> a <> "\n")
  & S.fold (FH.writeChunks stdout)


setValue :: Vector a -> (Int, a) -> Vector a
setValue vect (i,v) = V.modify (\vs -> MV.write vs i v) vect

initialStateFor :: (Foldable t, Monoid m) => t a -> Vector m
initialStateFor v = V.replicate (length v) mempty
